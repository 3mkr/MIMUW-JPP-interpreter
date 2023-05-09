
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

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> workWithFile [inputFile]
        _           -> noInput

workWithFile :: [FilePath] -> IO ()
workWithFile inputFile = do
    input <- readFile $ head inputFile
    case pProgram $ myLexer input of
        Left err -> do
            hPutStrLn stderr $ "Parsing Error: " ++ err
            exitFailure
        Right tree -> do
            --runTypeCheck
            result <- runEval tree
            case result of
                (Left err, _) -> do
                    hPutStrLn stderr $ "Runtime Error: " ++ err
                    exitFailure
                (Right _, store) -> do
                    putStrLn (show store)       -- TODEL
                    return ()
        
            --putStrLn $ "Jest git: " ++ show tree


noInput :: IO()
noInput = do
    hPutStrLn stderr "Error: There is no input to interpret."
    exitFailure



