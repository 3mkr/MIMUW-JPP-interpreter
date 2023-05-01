module Types where

import Data.Map

-- Prettier names
type Err        =   String
type Name       =   String
type Var        =   String

-- Prettier names for more advanced data types
data HintType   =   TInt | TString | TBool | TVoid | TTuple [HintType] deriving (Eq, Ord)
type EnvType    =   Map Name HintType
type Env        =   Map Var Int
type Store      =   Map Int HintType