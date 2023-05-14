module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Prelude

-- Grammar files import
import AbsHint
import ErrM
import PrintHint
import SkelHint

-- Utils import
import Types
import Errors
import UtilFunctions   

-- Starting monads with proper parameters
runEval :: Program -> [String] -> IO (Either RuntimeErr (), Store)
runEval prog vals = runEvalControl prog Map.empty Map.empty vals

runEvalControl :: Program -> Env -> Store -> [String] -> IO (Either RuntimeErr (), Store)
runEvalControl prog env store vals = runStateT (runExceptT (runReaderT (runMain prog vals) env)) store


-- Finding main() and running it
runMain :: Program -> [String] -> EvalControl ()
runMain (Program a topDefs) vals = do
    let findMain = filter isMain topDefs
    if null findMain
        then throwError noMainError
        else do
            let mainFunc = head $ findMain
            (nEnv, nStore) <- saveFunToEnv (Program a topDefs)
            put nStore
            evalFunctionMain mainFunc nEnv vals
            return ()

evalFunctionMain :: TopDef -> Env -> [String] -> EvalControl()
evalFunctionMain (FnDef _ _ _ args block) env vals = do
    let hintVals = convertToHint vals
    evalFunction (VFun args block) env hintVals
    return ()


-- Evaluating function definition
evalFunction :: HintValue -> Env -> [HintValue] -> EvalControl (Maybe StmtOutput)
evalFunction (VFun args block) env vals = do
    store <- get
    let newAddr     = Map.size env
    let argNames    = map (\(Arg _ _ (Ident name)) -> name) args
    let mapSize     = length argNames
    let newAddrs    = [newAddr..newAddr+mapSize] 
    let newEnv      = Map.fromList (zip argNames (map (\x -> (x, False)) newAddrs)) `Map.union` env
    let newStore    = Map.fromList (zip newAddrs vals) `Map.union` store
    put newStore

    result <- local (const newEnv) (evalBlock block)
    return result


-- Evaluating block of statements
evalBlock :: Block -> EvalControl (Maybe StmtOutput)
evalBlock (Block _ stmts) = do
    result <- evalBlockOfStmts stmts
    case result of
        Nothing             -> return Nothing
        Just LoopCont       -> return $ Just LoopCont
        Just LoopBreak      -> return $ Just LoopBreak
        Just (ReturnVal v)  -> return $ Just $ ReturnVal v

evalBlockOfStmts :: [Stmt] -> EvalControl (Maybe StmtOutput)
evalBlockOfStmts [] = return Nothing
evalBlockOfStmts (s : ss) = do
    result <- evalStmt s
    case result of
        Just (Environment newEnv)   -> local (const newEnv) $ evalBlockOfStmts ss
        Just LoopCont               -> return $ Just LoopCont
        Just LoopBreak              -> return $ Just LoopBreak
        Just (ReturnVal v)          -> return $ Just $ ReturnVal v
        Nothing                     -> evalBlockOfStmts ss


-- Evaluating single statement
evalStmt :: Stmt -> EvalControl (Maybe StmtOutput)


-- Print Statements
evalStmt (Print _ e) = do
    v <- evalExpr e
    liftIO $ putStrLn (show v)
    return Nothing

evalStmt (Printf loc e1 e2 e3 msg) = do
    (VArr v1) <- evalExpr e1
    (VArr v2) <- evalExpr e2
    (VArr v3) <- evalExpr e3
    line <- extractLine loc
    fullMsg <- createMsg line msg v1 v2 v3 []
    liftIO $ putStrLn (show fullMsg)
    return Nothing


-- If Statements
evalStmt (Cond _ e block) = do
    VBool b <- evalExpr e
    if b == True
        then do
            result <- evalBlock block
            case result of
                Just LoopCont  -> return $ Just LoopCont
                Just LoopBreak -> return $ Just LoopBreak 
                _ -> return Nothing
        else return Nothing

evalStmt (CondElse _ e blockT blockF) = do
    VBool b <- evalExpr e
    if b == True
        then do
            result <- evalBlock blockT
            case result of
                Just LoopCont  -> return $ Just LoopCont
                Just LoopBreak -> return $ Just LoopBreak
                _ -> return Nothing
        else do
            result <- evalBlock blockF
            case result of
                Just LoopCont  -> return $ Just LoopCont
                Just LoopBreak -> return $ Just LoopBreak
                _ -> return Nothing
    return Nothing


-- Loop Statements
evalStmt (While loc e block) = do
    VBool b <- evalExpr e
    if b == True
        then do
            result <- evalBlock block
            case result of
                Just LoopBreak -> return Nothing
                _ -> do
                    evalStmt (While loc e block)
                    return Nothing
        else return Nothing

evalStmt(BreakExp _) = do
    return $ Just LoopBreak

evalStmt(ContExp _) = do
    return $ Just LoopCont

evalStmt (For _ x e1 e2 block) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    runForLoop x v1 v2 block
    return Nothing


-- ++ and -- operators
evalStmt (Incr loc (Ident x)) = do
    line <- extractLine loc
    changeVIntVar x (+) (1) line
    return Nothing

evalStmt (Decr loc (Ident x)) = do
    line <- extractLine loc
    changeVIntVar x (-) (1) line
    return Nothing


-- Return statements
evalStmt (Ret _ e) = do
    v <- evalExpr e
    return $ Just $ ReturnVal v

evalStmt (VRet _) = return Nothing


-- Other Statements
evalStmt (SExp _ e) = do
    evalExpr e >> return Nothing

-- Reading input expression
evalStmt(Input loc (Ident x)) = do
    env <- ask
    store <- get
    v <- liftIO $ getLine
    let hintV = valToHint v
    line <- extractLine loc
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just (addr, ro) -> do
            if ro == False
                then do
                    put $ Map.insert addr hintV store
                    return Nothing
                else
                    throwError $ readOnlyVarError x (show line)

evalStmt (Empty _) = return Nothing


-- Variable declarations and assignments
evalStmt (Decl _ _ items) = do
    newEnv <- evalDeclare items
    return $ Just $ Environment newEnv

evalStmt (Ass loc (Ident x) e) = do
    env <- ask
    store <- get
    v <- evalExpr e
    line <- extractLine loc
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just (addr, ro) -> do
            if ro == False
                then do
                    put $ Map.insert addr v store
                    return Nothing
                else
                    throwError $ readOnlyVarError x (show line)

evalStmt (ArrAss loc (Ident x) idx e) = do
    env <- ask
    store <- get
    v <- evalExpr e
    line <- extractLine loc
    VInt i <- evalExpr idx
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just (addr, ro) -> do
            if ro == False
                then do
                    case Map.lookup addr store of
                            Just (VArr arr) -> do
                                let newArr = modifyArrAtIdx arr i v []
                                let newVal = VArr (newArr)
                                let newStore = Map.insert addr newVal store
                                put newStore
                                return Nothing
                            Nothing -> throwError $ noValError x (show line)
                            _ -> throwError $ wrongTypeError x "VArr" (show line)
                else
                    throwError $ readOnlyVarError x (show line)


changeVIntVar :: String -> (Int -> Int -> Int) -> Int -> Int -> EvalControl ()
changeVIntVar x op change line = do
    env <- ask
    store <- get
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just (addr, ro) -> do
            if ro == False
                then do
                    case Map.lookup addr store of
                        Just (VInt v) -> do
                            let newVal = VInt (op v change)
                            let newStore = Map.insert addr newVal store
                            put newStore
                            return ()
                        Nothing -> throwError $ noValError x (show line)
                        _ -> throwError $ wrongTypeError x "VInt" (show line)
                else
                    throwError $ readOnlyVarError x (show line)


runForLoop :: Ident -> HintValue -> HintValue -> Block -> EvalControl ()
runForLoop (Ident x) v1 v2 block = do
    env <- ask
    store <- get
    let addr = Map.size env
    let newEnv = Map.insert x (addr, True) env
    put $ Map.insert addr v1 store
    local (const newEnv) $ loopHelp x v1 v2 block addr
    return ()


loopHelp :: String -> HintValue -> HintValue -> Block -> Int -> EvalControl ()
loopHelp x v1 v2 block addr = do
    case (v1 <= v2) of
        True -> do
            result <- evalBlock block
            case result of
                Just LoopBreak -> return ()
                _ -> do
                    store <- get
                    put $ Map.insert addr (vIntAdd v1 1) store 
                    loopHelp x (vIntAdd v1 1) v2 block addr
                    return ()
        False -> return ()


evalDeclare :: [Item] -> EvalControl Env
evalDeclare (i : []) = do
    newEnv <- evalSingleDeclare i
    return newEnv
evalDeclare (i : is) = do 
    newEnv <- evalSingleDeclare i 
    newerEnv <- local (const newEnv) $ evalDeclare is
    return newerEnv


evalSingleDeclare :: Item -> EvalControl Env
evalSingleDeclare (Init _ (Ident x) e) = do
    env <- ask
    store <- get
    v <- evalExpr e
    case Map.lookup x env of
        Just (addr, ro) -> do
            let newAddr = Map.size env
            let newEnv = Map.insert x (newAddr, False) env
            put $ Map.insert newAddr v store       -- We already have variable with that name 
            return newEnv
        Nothing -> do
            let newAddr = Map.size env
            let newEnv = Map.insert x (newAddr, False) env
            put $ Map.insert newAddr v store
            return newEnv

evalSingleDeclare (NoInit loc (Ident x)) = do
    env <- ask
    store <- get
    line <- extractLine loc
    case Map.lookup x env of    
        Just addr -> throwError $ duplicateVarError x (show line)
        Nothing -> do
            let newAddr = Map.size env
            let newEnv = Map.insert x (newAddr, False) env
            return newEnv



-- Evaluating expression logic
evalExpr :: Expr -> EvalControl HintValue


-- Math expressions
evalExpr (ELitInt _ i) = do
    return (VInt (fromInteger i))

evalExpr (EAdd _ e1 op e2) = do
    VInt v1 <- evalExpr e1
    VInt v2 <- evalExpr e2
    return $ VInt (operationAdd op v1 v2)

evalExpr (EMul loc e1 op e2) = do
    VInt v1 <- evalExpr e1
    VInt v2 <- evalExpr e2
    line <- extractLine loc
    case op of
        Times _ -> return $ VInt (operationMul op v1 v2)
        Div _   -> if v2 == 0
                    then throwError $ divByZeroError (show line)
                    else return $ VInt (operationMul op v1 v2)
        Mod _   -> if v2 == 0
                    then throwError $ modZeroError (show line)
                    else return $ VInt (operationMul op v1 v2)

evalExpr (Neg _ e) = do
    VInt v <- evalExpr e
    return $ VInt $ -v


-- Logical expressions
evalExpr (ELitTrue _) = do
    return (VBool (True))

evalExpr (ELitFalse _) = do
    return (VBool (False))

evalExpr (ERel _ e1 rel e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    return $ VBool (comparasionL rel v1 v2)

evalExpr (EAnd _ e1 e2) = do
    VBool b1 <- evalExpr e1
    VBool b2 <- evalExpr e2
    return $ VBool (b1 && b2)

evalExpr (EOr _ e1 e2) = do
    VBool b1 <- evalExpr e1
    VBool b2 <- evalExpr e2
    return $ VBool (b1 || b2)

evalExpr (Not _ e) = do
    VBool b <- evalExpr e
    return $ VBool $ not b


-- String expression
evalExpr (EString _ s) = do
    return (VString (s))


-- Array & Tuple expressions
evalExpr (EArr _ es) = do
    v <- arrayCreator es
    return (VArr (v))

evalExpr (EArrIdx loc eArr eIdx) = do
    (VArr arr) <- evalExpr eArr
    (VInt idx) <- evalExpr eIdx
    line <- extractLine loc
    if idx >= 0 && idx < fromIntegral (length arr)
        then return $ arr !! fromIntegral idx
        else throwError $ indexOutOfBounds (show line)

evalExpr (ETuple _ es) = do
    v <- arrayCreator es
    return (VTuple (v))
    

-- Variable expressions
evalExpr (EVar loc (Ident x)) = do
    env <- ask
    store <- get
    line <- extractLine loc
    case Map.lookup x env of
        Just (addr, _) -> case Map.lookup addr store of
            Just val -> return val
            Nothing -> throwError $ noValError x (show line)
        Nothing -> throwError $ unknownVarError x (show line)


-- EApp expression
evalExpr (EApp loc (Ident x) es) = do
    env <- ask
    store <- get
    vals <- arrayCreator es
    line <- extractLine loc
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just (addr, _) -> case Map.lookup addr store of
            Nothing -> throwError $ noValError x (show line)
            Just (VFun args block) -> do
                r <- evalFunction (VFun args block) env vals
                case r of
                    Just (ReturnVal result) -> return result
                    _ -> return VVoid


-- Empty expression
--evalExpr (EEmpty _) = do
--    return VVoid


arrayCreator :: [Expr] -> EvalControl [HintValue]
arrayCreator [] = do
    return []
arrayCreator (e : []) = do
    v1 <- evalExpr e
    return $ [v1]
arrayCreator (e : es) = do
    v1 <- evalExpr e
    vR <- arrayCreator es
    return $ v1 : vR


extractLine :: Maybe (Int, Int) -> EvalControl Int
extractLine lData = case lData of
    Just (x, _) -> return x
    Nothing -> throwError "Unknown location!"