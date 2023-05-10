module Errors where

noMainError         = "There is no main function in this program!"
divByZeroError      = "Division by zero"
modZeroError        = "Modulo zero is undefined"
unknownVarError     = "Unknown variable "
noValError          = "No value for variable "
duplicateVarError   = "Variable by that name already exists"
indexOutOfBounds    = "Array index out of bounds"
invalidArrayAccess  = "Invalid array access"
readOnlyVarError x  = "Variable " ++ x ++ " is read-only, you can't modify it"