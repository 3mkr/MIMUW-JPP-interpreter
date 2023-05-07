module Types where

import Data.Map
import AbsHint

-- Prettier names
type RuntimeErr =   String
type Name       =   String
type Var        =   String
type Address    =   Int

-- Prettier names for more advanced data types
data HintType   =   TInt | TString | TBool | TVoid | TTuple [HintType] deriving (Eq, Ord, Show)
data HintValue  =   VInt Int | VString String | VBool Bool | VVoid deriving (Eq, Ord, Show)
--type EnvType    =   Map Name HintType
type Env        =   Map String Address
type Store      =   Map Address HintValue