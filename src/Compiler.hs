module Compiler
    ( readAndCompile
    ) where

import Parser

readAndCompile :: IO ()
<<<<<<< HEAD
readAndCompile = do  
    program <- readFile "D:/OneDrive/Dokumente/FHNW-MightyTower/cpib/CPIB_ILMCompiler/sample/sample1.iml"
=======
readAndCompile = do
    program <- readFile "/Users/benikm91/Documents/FHNW/Semester5/cpib/MyStuff/compiler/sample/sample1.iml"
>>>>>>> 9a7d6a754bca17830e71f11866e004a683ec069f
    -- "/Users/benikm91/Documents/FHNW/Semester5/cpib/MyStuff/compiler/sample/sample1.iml"
    -- "E:/Users/Christian/Documents/FHNW/Semester5/cpib/CPIB_ILMCompiler/sample/sample1.iml"
    -- "D:/OneDrive/Dokumente/FHNW-MightyTower/cpib/CPIB_ILMCompiler/sample/sample1.iml"
    putStrLn $ "compiling " ++ program
    putStrLn (printTree $ readExpr program)
    --putStrLn $ compile program

--compile = do y
  --program <- getLine
  --let res = compile program
	--putStrLn res

compile :: String -> String
compile s = show (readExpr $ s)