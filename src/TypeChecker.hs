module TypeChecker where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Prelude

-- Grammar files import
import AbsHint
import ErrM
import PrintHint
import SkelHint

-- Utils import
import Types
import Errors
import UtilFunctions 


-- Starting monads with proper parameters
runTypeCheck :: Program -> [String] -> IO (Either RuntimeErr ())
runTypeCheck prog vals = runTypeControl prog Map.empty vals

runTypeControl :: Program -> EnvType -> [String] -> IO (Either RuntimeErr ())
runTypeControl prog env vals = runExceptT (runReaderT (checkMain prog vals) env)


-- Finding main() and typechecking it
checkMain :: Program -> [String] -> TypeControl ()
checkMain (Program a topDefs) vals = do
    let findMain = filter isMain topDefs
    if null findMain
        then throwError noMainError
        else do
            let mainFunc = head $ findMain
            nEnv <- saveFunTypesToEnv (Program a topDefs)
            local (const nEnv) $ checkFunctionMain mainFunc vals
            return ()

checkFunctionMain :: TopDef -> [String] -> TypeControl()
checkFunctionMain (FnDef _ _ _ args block) vals = do
    let argNames    = map (\(Arg _ _ (Ident name)) -> name) args
    let argTypes    = map (\x-> makeArgHint x) args
    let actualTypes = convertToHintType vals
    areArgsCorrect <- compareArgs 1 actualTypes argTypes
    if areArgsCorrect == True
        then do
            checkFunction (TFun TVoid argNames argTypes block)
            return ()
        else throwError $ functionArgsErr (show 1)


-- Typechecking function definition
checkFunction :: HintType -> TypeControl (Maybe StmtType)
checkFunction (TFun retType names args block) = do
    env <- ask
    let newEnv  = Map.fromList (zip names args) `Map.union` env
    result <- local (const newEnv) (checkBlock block)
    return result


-- Typechecking block of statements
checkBlock :: Block -> TypeControl (Maybe StmtType)
checkBlock (Block _ stmts) = do
    result <- checkBlockOfStmts stmts
    case result of
        Nothing                 -> return Nothing
        Just (ReturnType t)     -> return $ Just $ ReturnType t

checkBlockOfStmts :: [Stmt] -> TypeControl (Maybe StmtType)
checkBlockOfStmts [] = return Nothing
checkBlockOfStmts (s : ss) = do
    result <- checkStmt s
    case result of
        Just (EnvironmentType newEnv)   -> local (const newEnv) $ checkBlockOfStmts ss
        Just TLoopCont                  -> checkBlockOfStmts ss
        Just TLoopBreak                 -> checkBlockOfStmts ss
        Just (ReturnType t)             -> return $ Just $ ReturnType t
        Nothing                         -> checkBlockOfStmts ss


-- Typechecking single statement
checkStmt :: Stmt -> TypeControl (Maybe StmtType)


-- Print Statements
checkStmt (Print _ e) = do
    checkExpr e
    return Nothing

checkStmt (Printf loc e1 e2 e3 msg) = do
    line <- extractLine loc
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    t3 <- checkExpr e3
    if t1 /= TArr TInt && t1 /= TArr TEmpty
        then throwError $ typesErr (show line) "TArr TInt" (show t1)
        else do
            if t2 /= TArr TString && t2 /= TArr TEmpty
                then throwError $ typesErr (show line) "TArr TString" (show t2)
                else do
                    if t3 /= TArr TBool && t3 /= TArr TEmpty
                        then throwError $ typesErr (show line) "TArr TBool" (show t3)
                        else return Nothing


-- If Statements
checkStmt (Cond loc e block) = do
    t <- checkExpr e
    line <- extractLine loc
    if t /= TBool
        then throwError $ typesErr (show line) "TBool" (show t)
        else do
            result <- checkBlock block
            return result

checkStmt (CondElse loc e blockT blockF) = do
    t <- checkExpr e
    line <- extractLine loc
    if t /= TBool
        then throwError $ typesErr (show line) "TBool" (show t)
        else do
            Just (ReturnType r1) <- checkBlock blockT
            Just (ReturnType r2) <- checkBlock blockF
            if r1 == r2
                then return $ Just $ ReturnType r1
                else throwError $ mulitpleReturnTypesErr (show line)


-- Loop Statements
checkStmt (While loc e block) = do
    t <- checkExpr e
    line <- extractLine loc
    if t /= TBool
        then throwError $ typesErr (show line) "TBool" (show t)
        else do
            checkBlock block
            return Nothing

checkStmt(BreakExp _) = do
    return $ Just TLoopBreak

checkStmt(ContExp _) = do
    return $ Just TLoopCont

checkStmt (For loc (Ident x) e1 e2 block) = do
    env <- ask
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    line <- extractLine loc
    if t1 == TInt
        then do
            if t2 == TInt
                then do
                    let newEnv = Map.insert x TInt env
                    local (const newEnv) $ checkBlock block
                    return $ Just $ EnvironmentType newEnv
                else throwError $ typesErr (show line) "TInt" (show t2)
        else throwError $ typesErr (show line) "TInt" (show t1)


-- ++ and -- operators
checkStmt (Incr loc (Ident x)) = testIfInt loc x 

checkStmt (Decr loc (Ident x)) = testIfInt loc x


-- Return statements
checkStmt (Ret _ e) = do
    t <- checkExpr e
    return $ Just $ ReturnType t

checkStmt (VRet _) = do
    return $ Just $ ReturnType TVoid


-- Other Statements
checkStmt (SExp _ e) = do
    checkExpr e >> return Nothing


-- Reading input expression
checkStmt (Input _ (Ident x)) = do
    return $ Just $ ReturnType TScan


-- Variable declarations and assignments
checkStmt (Decl loc typ items) = do
    newEnv <- checkDeclare items (makeTypeHint typ) loc
    return $ Just $ EnvironmentType newEnv

checkStmt (Ass loc (Ident x) e) = do
    env <- ask
    t <- checkExpr e
    line <- extractLine loc
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just oldType -> do
            if t == oldType
                then return Nothing
                else throwError $ typesErr (show line) (show oldType) (show t)

checkStmt (ArrAss loc (Ident x) idx e) = do
    env <- ask
    tArrV <- checkExpr e
    tIdx <- checkExpr idx
    line <- extractLine loc
    if tIdx /= TInt
        then throwError $ arrayIndexErr (show line)
        else do
            case Map.lookup x env of
                Just (TArr arr) -> do
                    if tArrV == arr
                        then return Nothing
                        else throwError $ typesErr (show line) (show arr) (show tArrV)
                Nothing -> throwError $ noValError x (show line)
                _ -> throwError $ wrongTypeError x "TArr" (show line)


checkStmt _ = do
    return Nothing

testIfInt :: Maybe(Int, Int) -> String -> TypeControl (Maybe StmtType)
testIfInt loc x = do
    env <- ask
    line <- extractLine loc
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just oldType -> do
            if oldType == TInt
                then return Nothing
                else throwError $ typesErr (show line) "TInt" (show oldType)

checkDeclare :: [Item] -> HintType -> Maybe (Int, Int) -> TypeControl EnvType
checkDeclare (i : []) lType loc = do
    newEnv <- checkSingleDeclare i lType loc
    return newEnv
checkDeclare (i : is) lType loc = do 
    newEnv <- checkSingleDeclare i lType loc
    newerEnv <- local (const newEnv) $ checkDeclare is lType loc
    return newerEnv

checkSingleDeclare :: Item -> HintType -> Maybe (Int, Int) -> TypeControl EnvType
checkSingleDeclare (Init _ (Ident x) e) expect loc = do
    env <- ask
    t <- checkExpr e
    line <- extractLine loc
    if (t == expect) || (t == TArr TEmpty && (expect == TArr TInt || expect == TArr TString || expect == TArr TBool)) 
        then do
            let newEnv = Map.insert x t env
            return newEnv
        else throwError $ varTypesErr (show line) (show expect) (show t) x

checkSingleDeclare (NoInit _ (Ident x)) expect loc = do
    env <- ask
    let newEnv = Map.insert x expect env
    return newEnv



-- Typechecking expression logic
checkExpr :: Expr -> TypeControl HintType


-- Math expressions
checkExpr(ELitInt _ i) = do
    return TInt

checkExpr (EAdd loc e1 _ e2) = do
    result <- check2Args loc e1 e2 TInt TInt TInt
    return result

checkExpr (EMul loc e1 _ e2) = do
    result <- check2Args loc e1 e2 TInt TInt TInt
    return result

checkExpr (Neg loc e) = do
    result <- check1Arg loc e TInt TInt
    return result


-- Logical expressions
checkExpr (ELitTrue _) = do
    return TBool

checkExpr (ELitFalse _) = do
    return TBool

checkExpr (ERel loc e1 _ e2) = do
    result <- check2Args loc e1 e2 TInt TInt TBool
    return result

checkExpr (EAnd loc e1 e2) = do
    result <- check2Args loc e1 e2 TBool TBool TBool
    return result

checkExpr (EOr loc e1 e2) = do
    result <- check2Args loc e1 e2 TBool TBool TBool
    return result

checkExpr(Not loc e) = do
    result <- check1Arg loc e TBool TBool
    return result


-- String expression
checkExpr (EString _ s) = do
    return TString


-- Array & Tuple expressions
checkExpr (EArr loc es) = do
    tList <- arrayCreator es
    if null tList
        then return (TArr (TEmpty))
        else do
            let t = head tList 
            isGoodType <- isSingleType t tList
            line <- extractLine loc
            if isGoodType /= TProblem
                then return (TArr (t))
                else throwError $ multipleArrayTypes (show line)

checkExpr (EArrIdx loc eArr eIdx) = do
    tArr <- checkExpr eArr
    tIdx <- checkExpr eIdx
    line <- extractLine loc
    if tIdx /= TInt
        then throwError $ arrayIndexErr (show line)
        else do
            case tArr of
                (TArr vType) -> return vType


-- Variable expressions
checkExpr (EVar loc (Ident x)) = do
    env <- ask
    line <- extractLine loc
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just varType -> return varType

-- EApp expression
checkExpr (EApp loc (Ident x) es) = do
    line <- extractLine loc
    ts <- arrayCreator es
    env <- ask
    case Map.lookup x env of
        Nothing -> throwError $ unknownVarError x (show line)
        Just (TFun retType argNames argTypes block) -> do
            areArgsCorrect <- compareArgs line ts argTypes
            if areArgsCorrect == True
                then do
                    r <- checkFunction (TFun retType argNames argTypes block)
                    case r of
                        Just (ReturnType actualRetType) -> do
                            if retType == actualRetType
                                then return retType
                                else throwError $ functionRetError (show line) (show retType) (show actualRetType)
                        _ -> return TVoid
                else throwError "Never thrown error"

checkExpr _ = do
    return TVoid

compareArgs :: Int -> [HintType] -> [HintType] -> TypeControl Bool
compareArgs _ [] [] = do
    return True
compareArgs line is [] = throwError $ functionArgsNoErr (show line)
compareArgs line [] es = throwError $ functionArgsNoErr (show line)
compareArgs line (i : is) (e : es) = do
    if i == e
        then compareArgs line is es
        else throwError $ functionArgsErr (show line)


check1Arg :: Maybe (Int, Int) -> Expr -> HintType -> HintType -> TypeControl HintType
check1Arg loc e expect result = do
    t <- checkExpr e
    line <- extractLine loc
    if t == expect
        then return result
        else throwError $ typesErr (show line) (show expect) (show t)

check2Args :: Maybe (Int, Int) -> Expr -> Expr -> HintType -> HintType -> HintType -> TypeControl HintType
check2Args loc e1 e2 expect1 expect2 result = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    line <- extractLine loc
    if t1 == expect1
        then if t2 == expect2
            then return result
            else throwError $ typesErr (show line) (show expect2) (show t2)
        else throwError $ typesErr (show line) (show expect1) (show t1)

arrayCreator :: [Expr] -> TypeControl [HintType]
arrayCreator [] = do
    return []
arrayCreator (e : []) = do
    t1 <- checkExpr e
    return $ [t1]
arrayCreator (e : es) = do
    t1 <- checkExpr e
    tR <- arrayCreator es
    return $ t1 : tR

isSingleType :: HintType -> [HintType] -> TypeControl HintType
isSingleType first [] = do
    return first
isSingleType first (t : ts) = do
    if first == t
        then isSingleType first ts
        else return TProblem

extractLine :: Maybe (Int, Int) -> TypeControl Int
extractLine lData = case lData of
    Just (x, _) -> return x
    Nothing -> throwError locationError