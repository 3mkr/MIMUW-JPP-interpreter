module Errors where

import Data.Map

-- Interpreter errors
noMainError                 = "There is no main function in this program!"
divByZeroError loc          = "Error in line " ++ loc ++ ": Division by zero"
modZeroError loc            = "Error in line " ++ loc ++ ": Modulo zero is undefined"
unknownVarError x loc       = "Error in line " ++ loc ++ ": Unknown variable " ++ x
noValError x loc            = "Error in line " ++ loc ++ ": Variable " ++ x ++ " has no value"
duplicateVarError x loc     = "Error in line " ++ loc ++ ": Variable " ++ x ++ " already exists"
indexOutOfBounds loc        = "Error in line " ++ loc ++ ": Array index out of bounds"
invalidArrayAccess loc      = "Error in line " ++ loc ++ ": Invalid array access"
readOnlyVarError x loc      = "Error in line " ++ loc ++ ": Variable " ++ x ++ " is read-only, you can't modify it"
wrongTypeError x eType loc  = "Error in line " ++ loc ++ ": Variable " ++ x ++ " has wrong type. Expected " ++ eType

-- TypeChecker errors
typesErr loc exp is         = "Error in line " ++ loc ++ ": Expected " ++ exp ++ " but got " ++ is
varTypesErr loc exp is x    = "Error in line " ++ loc ++ ": Variable " ++ x ++ " is of type " ++ exp ++ " but was assigned " ++ is
