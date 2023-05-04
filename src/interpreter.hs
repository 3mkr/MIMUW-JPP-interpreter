module Interpreter where

import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Prelude

-- Grammar files import
import AbsHint
import ErrM
import PrintHint
import SkelHint
import Types

type EvalControl a = (ReaderT Env (ExceptT RuntimeErr (StateT Store (IO)))) a

runEvalControl :: Program -> Env -> Store -> IO (Either RuntimeErr (), Store)
runEvalControl prog env store = runStateT (runExceptT (runReaderT (runStatements prog) env)) store

runEval :: Program -> IO (Either RuntimeErr (), Store)
runEval prog = runEvalControl prog empty empty

runStatements :: Program -> EvalControl ()
runStatements prog = do
    

{-
runStatementsHelp :: Program -> EvalControl ()
runStatementsHelp (s : []) = exec s
runStatementsHelp (s : ss) = exec s >> runStatementsHelp ss
-}
exec :: Stmt -> EvalControl ()
exec (Print _ exp) = do
    v <- eval exp
    liftIO $ putStrLn (show v)

eval :: Expr -> EvalControl HintValue
eval (ELitInt _ i) = do
    --return i
    --return $ fromInteger i
    return (VInt (fromInteger i))
  