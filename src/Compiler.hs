module Compiler
    ( readAndCompile
    ) where

import Parser

readAndCompile :: IO ()
readAndCompile = do  
    program <- readFile "/Users/benikm91/Documents/FHNW/Semester5/cpib/MyStuff/compiler/sample/sample1.iml"
    -- "E:\Users\Christian\Documents\FHNW\Semester5\cpib\CPIB_ILMCompiler\sample\sample1.iml"
    -- "D:\OneDrive\Dokumente\FHNW-MightyTower\cpib\CPIB_ILMCompiler\sample\sample1.iml"
    putStrLn $ "compiling " ++ program
    putStrLn $ compile program

--compile = do 
  --program <- getLine
  --let res = compile program
	--putStrLn res

compile :: String -> String
compile s = show (readExpr $ s)