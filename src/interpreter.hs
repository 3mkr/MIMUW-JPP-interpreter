module Interpreter where

import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

-- Grammar files import
import AbsHint
import ErrM
import PrintHint
import SkelHint
import Types

type EvalControl a = (ReaderT Env (ExceptT RuntimeErr (StateT Store (Identity)))) a

runEvalControl :: [Stmt] -> Env -> Store -> (Either RuntimeErr (), Store)
runEvalControl program env store = runIdentity (runStateT (runExceptT (runReaderT (runStatements program) env)) store)

runEval :: [Stmt] -> (Either RuntimeErr (), Store)
runEval p = runEvalControl p empty empty

runStatements :: [Stmt] -> EvalControl ()
runStatements stmt = runStatementsHelp stmt

runStatementsHelp :: [Stmt] -> EvalControl ()
runStatementsHelp (s : []) = exec s
runStatementsHelp (s : ss) = exec s >> runStatementsHelp ss

exec :: Stmt -> EvalControl ()
exec (Print _ exp) = do
  v <- eval exp
  liftIO $ putStrLn (show v)

eval :: Expr -> EvalControl HintValue
eval (ELitInt _ i) = do
  --return i
  --return $ fromInteger i
  return (VInt (fromInteger i))