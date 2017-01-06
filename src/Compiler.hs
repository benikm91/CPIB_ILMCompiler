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
    putStrLn $ program
    putStrLn "=============== Abstract Syntax Tree ==============="
    putStrLn $ printTree (readExpr program file)
    putStrLn "=============== Static Analysis ==============="
    putStrLn $ show $ check (readExpr program file)
    putStrLn "=============== Compiled Program ==============="
    putStrLn $ show (compile program file)
    putStrLn "=============== Run Program ==============="
    t <- debugProgramStack (compile program file)
    print t
    putStrLn "===================================================="

compile :: String -> String -> VMProgram
compile a b = toHaskellVM $ readExpr a b