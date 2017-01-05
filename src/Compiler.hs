module Compiler
    ( readAndCompile
    ) where

import Checker
import CodeGenerator
import Parser
import Path
import VirtualMachineIO

readAndCompile :: IO ()
readAndCompile = do
    program <- readFile getFilePath
    putStrLn "=============== Input Program ==============="
    putStrLn $ program
    putStrLn "=============== Abstract Syntax Tree ==============="
    putStrLn $ printTree (readExpr program)
    putStrLn "=============== Type Checker ==============="
    putStrLn $ show $ checkTypes (readExpr program)
    putStrLn "=============== Compiled Program ==============="
    putStrLn $ show (compile program)
    putStrLn "=============== Run Program ==============="
    t <- (debugProgramStack . compile) program
    print t
    putStrLn "===================================================="

compile :: String -> VMProgram
compile = toHaskellVM . readExpr