module Compiler
    ( readAndCompile
    ) where

import Parser

readAndCompile :: IO ()
readAndCompile = do  
    -- program <- getContents  
    putStrLn $ "compiling " -- ++ program
    putStrLn $ compile "program intDiv(in  const m:int64, in  const n:int64, out const q:int64, out const r:int64) {}"  

--compile = do 
  --program <- getLine
  --let res = compile program
	--putStrLn res

compile :: String -> String
compile s = show (readExpr $ s)
