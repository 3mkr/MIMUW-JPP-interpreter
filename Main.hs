
module Main where

import System.Environment ( getArgs )

import AbsHint
import ErrM
import PrintHint
import SkelHint
import ParHint
--import TestHint
import ErrM

import Types

main :: IO ()
main = do
    input <- readFile "example.txt"
    case pProgram $ myLexer input of
        Left err -> do
            putStrLn $ "Jakiś Error: " ++ err
        Right tree -> do
            putStrLn $ "Jest git"


