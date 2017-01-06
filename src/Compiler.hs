module Compiler
    ( readAndCompile
    ) where

import Checker
import CodeGenerator
import Parser
import VirtualMachineIO

readAndCompile :: String -> IO ()
readAndCompile file = do
    program <- readFile file
    putStrLn "=============== Input Program ==============="
    putStrLn program
    putStrLn "=============== Abstract Syntax Tree ==============="
    putStrLn $ printTree (readExpr program)
    putStrLn "=============== Static Analysis ==============="
    putStrLn $ show $ check (readExpr program)
    putStrLn "=============== Compiled Program ==============="
    putStrLn $ show (compile program)
    putStrLn "=============== Run Program ==============="
    t <- (execProgram . compile) program
    print t
    putStrLn "===================================================="

compile :: String -> VMProgram
compile = toHaskellVM . readExpr