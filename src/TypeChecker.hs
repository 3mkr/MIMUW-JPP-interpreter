import Data.Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

import Types

type TypeControl a = (ReaderT EnvType (ExceptT Err Identity)) a

evalExpressionType :: 

evalInstructionType :: 