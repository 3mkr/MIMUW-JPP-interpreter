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

type EvalControl a = (ReaderT Env (ExceptT RuntimeErr (StateT Store (IO)))) a

-- Starting monads with proper parameters
runEval :: Program -> IO (Either RuntimeErr (), Store)
runEval prog = runEvalControl prog Map.empty Map.empty

runEvalControl :: Program -> Env -> Store -> IO (Either RuntimeErr (), Store)
runEvalControl prog env store = runStateT (runExceptT (runReaderT (runMain prog) env)) store

-- Finding mian() and running it
runMain :: Program -> EvalControl ()
runMain (Program _ topDefs) = do
    let mainFunc = head $ filter isMain topDefs
    env <- ask
    evalFunction mainFunc env
    --store <- get                      -- TODEL
    --liftIO $ putStrLn (show store)    -- TODEL

isMain :: TopDef -> Bool
isMain (FnDef _ _ (Ident "main") _ _) = True
isMain _ = False

-- Evaluating function definition
evalFunction :: TopDef -> Env -> EvalControl ()
evalFunction (FnDef _ _ _ args block) env = do
    store <- get
    let newAddr = Map.size store
    --let newEnv = Map.insert x newAddr env
    --put $ Map.insert newAddr v store
    let argNames    = map (\(Arg _ _ (Ident name)) -> name) args
    let mapSize     = length argNames
    let newAddrs    = [newAddr..newAddr+mapSize] 
    --let argValues   = map (const (\ x -> x)) args                                 -- ?
    let newEnv      = Map.fromList (zip argNames newAddrs) `Map.union` env     -- ?
    local (\ _ -> newEnv) (evalBlock block)
    --evalBlock block

-- Evaluating block of statements
evalBlock :: Block -> EvalControl ()
evalBlock (Block _ stmts) = do
    let evalStmts = map evalStmt stmts
    results <- sequence evalStmts
    case results of   -- Return the value of the last statement
        [] -> return ()
        _ -> return (last results)

-- Evaluating single statement
evalStmt :: Stmt -> EvalControl ()
evalStmt (Print _ e) = do
    v <- evalExpr e
    liftIO $ putStrLn (show v)

evalStmt (Cond _ e block) = do
    VBool b <- evalExpr e
    if b == True
        then evalBlock block
        else return ()

evalStmt (CondElse _ e blockT blockF) = do
    VBool b <- evalExpr e
    if b == True
        then evalBlock blockT
        else evalBlock blockF

evalStmt (While a e block) = do --TOTEST
    VBool b <- evalExpr e
    if b == True
        then evalBlock block >> evalStmt (While a e block)
        else return ()

evalStmt (Incr _ (Ident x)) = changeVIntVar x (+) (1)

evalStmt (Decr _ (Ident x)) = changeVIntVar x (-) (1)

evalStmt (SExp _ e) = do
    evalExpr e >> return ()

evalStmt (Empty _) = return ()

evalStmt (VRet _) = return ()

evalStmt (Decl _ _ items) = do
    evalDeclare items
    return ()

changeVIntVar :: String -> (Int -> Int -> Int) -> Int -> EvalControl ()
changeVIntVar x op change = do
    env <- ask
    store <- get
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError
        Just addr -> case Map.lookup addr store of
            Nothing -> throwError $ unknownVarError
            Just val -> do
                let updatedVal = case val of
                                VInt i -> VInt (op i change)
                                _ -> error "Expected VInt"
                modify $ Map.insert addr updatedVal

evalDeclare :: [Item] -> EvalControl ()
evalDeclare [] = return ()
evalDeclare (i : []) = evalSingleDecl i
evalDeclare (i : is) = do 
    evalSingleDecl i 
    evalDeclare is

evalSingleDecl :: Item -> EvalControl ()
evalSingleDecl (Init _ (Ident x) e) = do
    env <- ask
    store <- get
    v <- evalExpr e
    case Map.lookup x env of    
        Just addr -> put $ Map.insert addr v store  -- We already have variable with that name
        Nothing -> do
            let newAddr = Map.size store
            let newEnv = Map.insert x newAddr env
            put $ Map.insert newAddr v store
            local (const newEnv) $ return ()


-- Extra functions for evaluating logical and mathematical expressions
operationAdd (Plus _)   e1 e2 = e1 + e2
operationAdd (Minus _)  e1 e2 = e1 - e2

operationMul (Times _)  e1 e2 = e1 * e2
operationMul (Div _)    e1 e2 = div e1 e2
operationMul (Mod _)    e1 e2 = e1 `mod` e2

comparasionL (LTH _)    e1 e2 = e1 <  e2
comparasionL (LE _)     e1 e2 = e1 <= e2
comparasionL (GTH _)    e1 e2 = e1 >  e2
comparasionL (GE _)     e1 e2 = e1 >= e2
comparasionL (EQU _)    e1 e2 = e1 == e2
comparasionL (NE _)     e1 e2 = e1 /= e2


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

-- Variable expressions
{-
evalExpr (EVar _ (Ident x)) = do
    r <- ask
    if Map.member x r
        then do --return $ VInt $ (r Map.! x)
            address <- r Map.! x
            s <- get
            if Map.member address s
                then return (s Map.! address)
                else throwError (noValError ++ show x)
        else throwError (unknownVarError ++ show x)
-}
evalExpr (EVar _ (Ident x)) = do
    env <- ask
    store <- get
    case Map.lookup x env of
        Just addr -> case Map.lookup addr store of
            Just val -> return val
            Nothing -> return $ VInt $ -8--throwError noValError
        Nothing -> return $ VInt $ 8 --throwError unknownVarError


-- EApp expressions