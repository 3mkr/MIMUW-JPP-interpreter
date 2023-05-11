module Types where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import AbsHint

-- Prettier names
type RuntimeErr =   String
type Name       =   String
type Var        =   String
type Address    =   Int
type ReadOnly   =   Bool

-- Prettier names for more advanced data types
data HintType   =   TInt | TString | TBool | TVoid | TTuple [HintType] deriving (Eq, Ord, Show)
data HintValue
    = VInt Int
    | VString String
    | VBool Bool
    | VVoid
    | VArr [HintValue]
    | VTuple [HintValue]
    | VFun [Arg] Block
    deriving (Eq, Ord, Show)
data StmtOutput=   Environment Env | ReturnVal HintValue | LoopCont | LoopBreak

-- Typechecker Environment
type EnvType    =   Map.Map Name HintType

-- Interpreter Environment & Store
type Env        =   Map.Map String (Address, ReadOnly)
type Store      =   Map.Map Address HintValue

-- Interpreter Monad
type EvalControl a = (ReaderT Env (ExceptT RuntimeErr (StateT Store (IO)))) a



