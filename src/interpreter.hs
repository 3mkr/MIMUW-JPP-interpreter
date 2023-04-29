import Data.Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity



-- import <powstały z BNFC parser>


-- powstałe z BNFC data Abstract Syntax Tree:
data Expr =
    EVar String
  | EInt Int
  | EAdd Expr Expr
  | ELet String Expr Expr
  

data Inst =
    EAs String Expr
  | EIf Expr Inst Inst

data Program =
    Prog [Inst]

type Var = String

type Env = Map Var Int

type Err = String


-- monada zaprojektowana do obliczania wyrażeń
-- EvalMonad a === Env -> (Either Err a)
type EvalMonad a = (ReaderT Env (ExceptT Err Identity)) a


runEvalMonad :: (EvalMonad a) -> Env -> Either Err a
runEvalMonad v r = runIdentity (runExceptT (runReaderT v r))


eval :: Expr -> EvalMonad Int

eval (EVar x) = do {
  r <- ask; -- ask zwraca (EvalMonad Env) które rozpakowujemy do r :: Env
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

runEval e = runEvalMonad (eval e) empty


err1 = runEval (ELet "x" (EInt 5) (EAdd (EVar "y") (EInt 7)))
-- err1 = Left "Unknown variable y at some place!"
val1 = runEval (ELet "x" (EInt 5) (EAdd (EVar "x") (EInt 7)))
-- val1 = Right 12