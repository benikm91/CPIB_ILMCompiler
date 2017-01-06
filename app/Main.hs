module Main where

import Compiler
import System.Environment
import Path

main = do
    args <- getArgs
    readAndCompile $ head args

debug = readAndCompile getFilePath