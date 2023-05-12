module UtilFunctions where

import qualified Data.Map as Map
import System.Exit          (exitFailure)
import System.IO
import Data.Char (isDigit)
import Data.Array

import AbsHint
import ErrM
import PrintHint
import SkelHint

import Types

noInput :: IO()
noInput = do
    hPutStrLn stderr "Error: There is no input to interpret."
    exitFailure

tupleToVFun :: ([Arg], Block) -> HintValue
tupleToVFun (x, y) = VFun x y

saveFunToEnv :: Program -> EvalControl (Env, Store)
saveFunToEnv (Program _ topDefs) = do 
    let allFunc  = filter (isNotMain) topDefs
    let fNames   = map (\(FnDef _ _ (Ident name) _ _) -> name) allFunc
    let fArgs    = map (\(FnDef _ _ _ args _) -> args) allFunc
    let fBlocks  = map (\(FnDef _ _ _ _ block) -> block) allFunc
    let fDef     = map tupleToVFun (zip fArgs fBlocks)
    let newAddrs = [0..(length fNames)]
    let newEnv   = Map.fromList (zip fNames (map (\x -> (x, False)) newAddrs))
    let newStore = Map.fromList (zip newAddrs fDef) 
    return (newEnv, newStore)

isMain :: TopDef -> Bool
isMain (FnDef _ _ (Ident "main") _ _) = True
isMain _ = False

isNotMain :: TopDef -> Bool
isNotMain (FnDef _ _ (Ident "main") _ _) = False
isNotMain _ = True

isAllDigits :: String -> Bool
isAllDigits s = all isDigit s

convertToHint :: [String] -> [HintValue]
convertToHint ss = convertHelp ss []

convertHelp :: [String] -> [HintValue] -> [HintValue]
convertHelp [] acc = acc
convertHelp (s : ss) acc = convertHelp ss ((valToHint s) : acc)

valToHint :: String -> HintValue
valToHint s
    | all isDigit s = VInt $ read s
    | head s == '*' && last s == '*' = VString (init (tail s))
    | s == "true" = VBool True
    | s == "false" = VBool False


createMsg :: String -> [HintValue] -> [HintValue] -> [HintValue] -> String -> String
createMsg [] _ _ _ acc = reverse acc
createMsg ('%' : 'd' : ms) (i : is) ss bs acc   = createMsg ms is ss bs ((reverse (show i)) ++ acc)
createMsg ('%' : 's' : ms) is (s : ss) bs acc   = createMsg ms is ss bs ((reverse (show s)) ++ acc)
createMsg ('%' : 'b' : ms) is ss (b : bs) acc   = createMsg ms is ss bs ((reverse (show b)) ++ acc)
createMsg (m : ms) is ss bs acc                 = createMsg ms is ss bs (m : acc)

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
