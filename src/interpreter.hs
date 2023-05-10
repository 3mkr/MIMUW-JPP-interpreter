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
runEval :: Program -> IO (Either RuntimeErr (), Store)
runEval prog = runEvalControl prog Map.empty Map.empty

runEvalControl :: Program -> Env -> Store -> IO (Either RuntimeErr (), Store)
runEvalControl prog env store = runStateT (runExceptT (runReaderT (runMain prog) env)) store

-- Finding main() and running it
runMain :: Program -> EvalControl ()
runMain (Program _ topDefs) = do
    let mainFunc = head $ filter isMain topDefs
    env <- ask
    evalFunction mainFunc env
    return ()



-- Evaluating function definition
evalFunction :: TopDef -> Env -> EvalControl (Maybe HintValue)
evalFunction (FnDef _ _ _ args block) env = do
    store <- get
    let newAddr     = Map.size store
    let argNames    = map (\(Arg _ _ (Ident name)) -> name) args
    let mapSize     = length argNames
    let newAddrs    = [newAddr..newAddr+mapSize] 
    let newEnv      = Map.fromList (zip argNames (map (\x -> (x, False)) newAddrs)) `Map.union` env     -- add argValues?
    result <- local (const newEnv) (evalBlock block)
    return result

-- Evaluating block of statements
evalBlock :: Block -> EvalControl (Maybe HintValue)
evalBlock (Block _ stmts) = do
    results <- evalBlockOfStmts stmts
    case results of
        Nothing -> return Nothing
        Just (ReturnVal v) -> return $ Just $ v

evalBlockOfStmts :: [Stmt] -> EvalControl (Maybe StmtOutput)
evalBlockOfStmts [] = return Nothing
evalBlockOfStmts (s : ss) = do
    result <- evalStmt s
    case result of
        Just (Environment newEnv) -> local (const newEnv) $ evalBlockOfStmts ss
        Just v -> return $ Just v
        Nothing -> evalBlockOfStmts ss

-- Evaluating single statement
evalStmt :: Stmt -> EvalControl (Maybe StmtOutput)

-- Print Statements
evalStmt (Print _ e) = do
    v <- evalExpr e
    liftIO $ putStrLn (show v)
    return Nothing

--evalStmt (Printf _ e1 e2 e3 msg) = do


-- If Statements
evalStmt (Cond _ e block) = do
    VBool b <- evalExpr e
    if b == True
        then do
            evalBlock block
            return Nothing
        else return Nothing

evalStmt (CondElse _ e blockT blockF) = do
    VBool b <- evalExpr e
    if b == True
        then evalBlock blockT
        else evalBlock blockF
    return Nothing

-- Loop Statements
evalStmt (While a e block) = do
    VBool b <- evalExpr e
    if b == True
        then do
            evalBlock block
            evalStmt (While a e block)
            return Nothing
        else return Nothing

evalStmt (For _ x e1 e2 block) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    runForLoop x v1 v2 block
    return Nothing

-- ++ and -- operators
evalStmt (Incr _ (Ident x)) = do
    changeVIntVar x (+) (1)
    return Nothing

evalStmt (Decr _ (Ident x)) = do
    changeVIntVar x (-) (1)
    return Nothing

-- Return statements
evalStmt (Ret _ e) = do
    v <- evalExpr e
    return $ Just $ ReturnVal v

evalStmt (VRet _) = return Nothing

evalStmt (SExp _ e) = do
    evalExpr e >> return Nothing

evalStmt (Empty _) = return Nothing

-- Variable declarations and assignments
evalStmt (Decl _ _ items) = do
    newEnv <- evalDeclare items
    return $ Just $ Environment newEnv

evalStmt (Ass _ (Ident x) e) = do
    env <- ask
    store <- get
    v <- evalExpr e
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError ++ x
        Just (addr, ro) -> do
            if ro == False
                then do
                    put $ Map.insert addr v store
                    return Nothing
                else
                    throwError $ readOnlyVarError x


runForLoop :: Ident -> HintValue -> HintValue -> Block -> EvalControl ()
runForLoop (Ident x) v1 v2 block = do
    env <- ask
    store <- get
    let addr = Map.size store
    let newEnv = Map.insert x (addr, True) env
    put $ Map.insert addr v1 store
    local (const newEnv) $ loopHelp x v1 v2 block addr
    return ()

loopHelp :: String -> HintValue -> HintValue -> Block -> Int -> EvalControl ()
loopHelp x v1 v2 block addr = do
    case (v1 <= v2) of
        True -> do
            evalBlock block
            store <- get
            put $ Map.insert addr (vIntAdd v1 1) store 
            loopHelp x (vIntAdd v1 1) v2 block addr
            return ()
        False -> return ()

changeVIntVar :: String -> (Int -> Int -> Int) -> Int -> EvalControl ()
changeVIntVar x op change = do
    env <- ask
    store <- get
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError
        Just (addr, ro) -> 
            if ro == False
                then do
                    case Map.lookup addr store of
                        Nothing -> throwError $ unknownVarError
                        Just val -> do
                            let updatedVal = case val of
                                            VInt i -> VInt (op i change)
                                            _ -> error "Expected VInt"
                            modify $ Map.insert addr updatedVal
                else throwError $ readOnlyVarError x

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
    case Map.lookup x env of    --ro
        Just (addr, ro) -> do
            put $ Map.insert addr v store  -- We already have variable with that name
            return env
        Nothing -> do
            let newAddr = Map.size store
            let newEnv = Map.insert x (newAddr, False) env
            put $ Map.insert newAddr v store
            return newEnv

evalSingleDeclare (NoInit _ (Ident x)) = do
    env <- ask
    store <- get
    case Map.lookup x env of    
        Just addr -> throwError $ duplicateVarError ++ x
        Nothing -> do
            let newAddr = Map.size store
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

evalExpr (EMul _ e1 op e2) = do
    VInt v1 <- evalExpr e1
    VInt v2 <- evalExpr e2
    case op of
        Times _ -> return $ VInt (operationMul op v1 v2)
        Div _   -> if v2 == 0
                    then throwError divByZeroError
                    else return $ VInt (operationMul op v1 v2)
        Mod _   -> if v2 == 0
                    then throwError modZeroError
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
    b1 <- evalExpr e1
    b2 <- evalExpr e2
    return $ VBool (comparasionL rel b1 b2)

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

evalExpr (EArrIdx _ eArr eIdx) = do
    (VArr arr) <- evalExpr eArr
    (VInt idx) <- evalExpr eIdx
    if idx >= 0 && idx < fromIntegral (length arr)
        then return $ arr !! fromIntegral idx
        else throwError indexOutOfBounds

evalExpr (ETuple _ es) = do
    v <- arrayCreator es
    return (VTuple (v))
    
-- Variable expressions
evalExpr (EVar _ (Ident x)) = do
    env <- ask
    store <- get
    case Map.lookup x env of
        Just (addr, _) -> case Map.lookup addr store of
            Just val -> return val
            Nothing -> throwError $ noValError ++ x
        Nothing -> throwError $ unknownVarError ++ x


-- EApp expressions

arrayCreator :: [Expr] -> EvalControl [HintValue]
arrayCreator (e : []) = do
    v1 <- evalExpr e
    return $ [v1]
arrayCreator (e : es) = do
    v1 <- evalExpr e
    vR <- arrayCreator es
    return $ v1 : vR