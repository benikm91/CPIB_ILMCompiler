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
    -- "/Users/benikm91/Documents/FHNW/Semester5/cpib/MyStuff/compiler/sample/sample1.iml"
    -- "E:/Users/Christian/Documents/FHNW/Semester5/cpib/CPIB_ILMCompiler/sample/sample1.iml"
    -- "D:/OneDrive/Dokumente/FHNW-MightyTower/cpib/CPIB_ILMCompiler/sample/sample1.iml"
    putStrLn "=============== Input Program ==============="
    putStrLn $ program
    putStrLn "=============== Abstract Syntax Tree ==============="
    putStrLn $ printTree (readExpr program)
    putStrLn "=============== Static Analysis ==============="
    putStrLn $ show $ check (readExpr program)
    putStrLn "=============== Compiled Program ==============="
    putStrLn $ show (compile program)
    putStrLn "=============== Run Program ==============="
    t <- (debugProgramStack . compile) program
    print t
    putStrLn "===================================================="
    --putStrLn $ compile program

--compile = do y
  --program <- getLine
  --let res = compile program
    --putStrLn res

compile :: String -> VMProgram
compile = toHaskellVM . readExpr