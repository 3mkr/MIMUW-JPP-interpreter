module UtilFunctions where

import qualified Data.Map as Map

import AbsHint
import ErrM
import PrintHint
import SkelHint

import Types

isMain :: TopDef -> Bool
isMain (FnDef _ _ (Ident "main") _ _) = True
isMain _ = False


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

vIntAdd :: HintValue -> Int -> HintValue
vIntAdd (VInt v) i = (VInt (v + i))
