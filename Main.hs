
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

workWithFile :: [FilePath] -> IO()
workWithFile inputFile = do
    input <- readFile $ head inputFile
    case pProgram $ myLexer input of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            exitFailure
        Right tree -> do
            let Program p = tree
            --runTypeCheck
            result <- runEval p
            --putStrLn $ "Jest git: " ++ show tree


noInput :: IO()
noInput = do
    hPutStrLn stderr "Error: There is no input to interpret."
    exitFailure



