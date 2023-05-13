
module Main where

import System.Environment   (getArgs)
import System.Exit          (exitFailure)
import System.IO

import AbsHint
import PrintHint
import SkelHint
import ParHint
import ErrM

import Types
import Interpreter
import TypeChecker
import UtilFunctions

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inputFile : rest) -> workWithFile [inputFile] rest
        _           -> noInput

workWithFile :: [FilePath] -> [String] -> IO ()
workWithFile inputFile vals = do
    input <- readFile $ head inputFile
    case pProgram $ myLexer input of
        Left err -> do
            hPutStrLn stderr $ "Parsing Error: " ++ err
            exitFailure
        Right tree -> do
            typeCheckResult <- runTypeCheck tree vals
            case typeCheckResult of
                (Left err) -> do
                    hPutStrLn stderr $ "TypeCheck Error: " ++ err
                    exitFailure
                (Right _) -> do
                    result <- runEval tree vals
                    case result of
                        (Left err, _) -> do
                            hPutStrLn stderr $ "Runtime Error: " ++ err
                            exitFailure
                        (Right _, store) -> do
                            return ()


{-

workWithFile :: [FilePath] -> [String] -> IO ()
workWithFile inputFile vals = do
    input <- readFile $ head inputFile
    case pProgram $ myLexer input of
        Left err -> do
            hPutStrLn stderr $ "Parsing Error: " ++ err
            exitFailure
        Right tree -> do
            --runTypeCheck
            result <- runEval tree vals
            case result of
                (Left err, _) -> do
                    hPutStrLn stderr $ "Runtime Error: " ++ err
                    exitFailure
                (Right _, store) -> do
                    putStrLn (show store)       -- TODEL
                    return ()
-}




