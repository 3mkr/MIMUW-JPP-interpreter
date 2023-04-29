import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity


import <powstały z BNFC parser>

data Expr =
    EVar String
  | EInt Int
  | EAdd Expr Expr
  | ELet String Expr Expr
  
{-
BNFC:

EAssign. Inst ::= Ident ":=" Expr
EIf. Inst ::= "if" Expr "then" Inst "else" Inst

Prog.  Program ::= [Inst]

entry Program
-}

data Inst =
    EAssign String Expr
  | EIf Expr Inst Inst

data Program =
    Prog [Inst]
    
    
-- runParser :: String -> Either String Program


type Env = Map String Int

type MyMonad a = (ReaderT Env (ExceptT String Identity)) a

eval :: Expr -> MyMonad Int

eval (EVar x) = do
  r <- ask
  if member x r then
    return (r ! x)
  else
    throwError ("Unknown variable " ++ x ++ " at some place!")

eval (EInt i) = do
  return i

eval (EAdd e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)


eval (ELet x e e1) = do
  v <- eval e
  v1 <- local (\ r -> insert x v r) (eval e1)
  return v1


runMyMonad :: Expr -> Either String Int

runMyMonad e = runIdentity $ runExceptT $ runReaderT (eval e) empty

{-
main s =
  let e = runParser s in
  case runMyMonad e of
    Left err -> show error somehow
    Right i -> show success somehow
-}



