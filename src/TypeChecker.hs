import Data.Map

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Prelude

import Types

type TypeControl a = (ReaderT EnvType (ExceptT Err IO)) a

evalExpressionType :: 

evalInstructionType :: 