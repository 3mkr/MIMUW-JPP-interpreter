module Errors where

import Data.Map

noMainError         = "There is no main function in this program!"
divByZeroError      = "Division by zero"
modZeroError        = "Modulo zero is undefined"
unknownVarError x   = "Unknown variable " ++ x
noValError x        = "Variable " ++ x ++ " has no value"
duplicateVarError   = "Variable by that name already exists"
indexOutOfBounds    = "Array index out of bounds"
invalidArrayAccess  = "Invalid array access"
readOnlyVarError x  = "Variable " ++ x ++ " is read-only, you can't modify it"
wrongTypeNotVInt x  = "Variable " ++ x ++ " has wrong type. Expected VInt."