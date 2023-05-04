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
    store <- get
    evalFunction mainFunc env store

isMain :: TopDef -> Bool
isMain (FnDef _ _ (Ident "main") _ _) = True
isMain _ = False

-- Evaluating function definition
evalFunction :: TopDef -> Env -> Store -> EvalControl ()
evalFunction (FnDef _ _ _ args block) env store = do
    let argNames    = map (\(Arg _ _ (Ident name)) -> name) args                -- ? 
    let argValues   = map (const (VInt 0)) args                                 -- ?
    let newEnv      = Map.fromList (zip argNames argValues) `Map.union` env     -- ?
    local (\ _ -> newEnv) (evalBlock block store)

-- Evaluating block of statements
evalBlock :: Block -> Store -> EvalControl ()
evalBlock (Block _ stmts) store = do
  -- Evaluate the statements in the block
  let evalStmts = map evalStmt stmts
  results <- sequence evalStmts
  -- Return the value of the last statement
  case results of
    [] -> return ()
    _ -> return (last results)

-- Evaluating single statement
evalStmt :: Stmt -> EvalControl ()
evalStmt (Print _ exp) = do
    v <- evalExpr exp
    liftIO $ putStrLn (show v)

evalStmt _ = return ()  -- TODO?


operationAdd (Plus _)   e1 e2 = e1 + e2
operationAdd (Minus _)  e1 e2 = e1 - e2

operationMul (Times _)  e1 e2 = e1 * e2
operationMul (Div _)    e1 e2 = div e1 e2
operationMul (Mod _)    e1 e2 = e1 `mod` e2

comparasionL (LTH _)    e1 e2 = e1 <  e1
comparasionL (LE _)     e1 e2 = e1 <= e1
comparasionL (GTH _)    e1 e2 = e1 >  e1
comparasionL (GE _)     e1 e2 = e1 >= e1
comparasionL (EQU _)    e1 e2 = e1 == e1
comparasionL (NE _)     e1 e2 = e1 /= e1


-- Evaluating expression
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

-- Logical expressions
evalExpr (ERel _ e1 rel e2) = do
    VBool b1 <- evalExpr e1
    VBool b2 <- evalExpr e2
    return $ VBool (comparasionL rel b1 b2)

evalExpr (EAnd _ e1 e2) = do
    VBool b1 <- evalExpr e1
    VBool b2 <- evalExpr e2
    return $ VBool (b1 && b2)

evalExpr (EOr _ e1 e2) = do
    VBool b1 <- evalExpr e1
    VBool b2 <- evalExpr e2
    return $ VBool (b1 || b2)

  