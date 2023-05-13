module Types where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import AbsHint

-- Prettier names
type RuntimeErr     =   String
type TypeCheckErr   =   String
type Name           =   String
type Var            =   String
type Address        =   Int
type ReadOnly       =   Bool

-- Types for TypeChecker
data HintType
    = TInt
    | TString
    | TBool
    | TVoid
    | TArr HintType
    | TTuple [HintType]
    | TFun HintType [HintType] Block
    | TScan
    deriving (Eq, Ord, Show)

-- Result of Stmt in TypeChecker
data StmtType = EnvironmentType EnvType | ReturnType HintType | TLoopCont | TLoopBreak

-- Value types for Interpreter
data HintValue
    = VInt Int
    | VString String
    | VBool Bool
    | VVoid
    | VArr [HintValue]
    | VTuple [HintValue]
    | VFun [Arg] Block
    deriving (Eq, Ord, Show)

-- Result of Stmt in Interpreter
data StmtOutput = Environment Env | ReturnVal HintValue | LoopCont | LoopBreak

-- Typechecker Environment
type EnvType = Map.Map Name HintType

-- Typechecker Monad
type TypeControl a = (ReaderT EnvType (ExceptT TypeCheckErr (IO))) a

-- Interpreter Environment & Store
type Env    =   Map.Map Name (Address, ReadOnly)
type Store  =   Map.Map Address HintValue

-- Interpreter Monad
type EvalControl a = (ReaderT Env (ExceptT RuntimeErr (StateT Store (IO)))) a



