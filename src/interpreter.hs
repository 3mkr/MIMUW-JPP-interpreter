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
--import TestHint

import Types



-- import <powstały z BNFC parser>


-- powstałe z BNFC data Abstract Syntax Tree:
--data Expr =
--    EVar String
--  | EInt Int
--  | EAdd Expr Expr
--  | ELet String Expr Expr
  

--data Inst =
--    EAs String Expr
--  | EIf Expr Inst Inst

--data Program =
--    Prog [Inst]

-- monada zaprojektowana do obliczania wyrażeń
-- EvalControl a === Env -> (Either Err a)
type EvalControl a = (ReaderT Env (ExceptT Err ( StateT Store (Identity)))) a


runEvalControl :: (EvalControl a) -> Env  -> Store -> Either Err a
runEvalControl v r store = runIdentity (runStateT (runExceptT (runReaderT v r)) store)


eval :: Expr -> EvalControl Int

eval (EVar x) = do {
  r <- ask; -- ask zwraca (EvalControl Env) które rozpakowujemy do r :: Env
  if member x r then
    return (r ! x)        -- (r ! x) :: Int
  else
    throwError ("Unknown variable " ++ x ++ " at some place!")
}

eval (EInt i) = do
  return i
  
eval (EAdd e1 e2) = do
  v1 <- eval e1 -- v1 :: Int
  v2 <- eval e2
  return (v1 + v2)
  
eval (ELet x e e1) = do
  v <- eval e
  v1 <- local (\ r -> insert x v r) (eval e1) -- oblicz (eval e1) w zmienionym srodowisku
  return v1

  
-- uruchom eval na danym Expr w pustym srodowisku
runEval :: Expr -> Either Err Int

runEval e = runEvalControl (eval e) empty


err1 = runEval (ELet "x" (EInt 5) (EAdd (EVar "y") (EInt 7)))
-- err1 = Left "Unknown variable y at some place!"
val1 = runEval (ELet "x" (EInt 5) (EAdd (EVar "x") (EInt 7)))
-- val1 = Right 12