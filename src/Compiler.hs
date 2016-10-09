module Compiler
    ( someFunc
    ) where

import Scanner

someFunc :: IO ()
someFunc = do 
	program <- getLine
	let res = compile program
	putStrLn res

compile :: String -> String
compile s = concat $ map (( "(" ++) .(++ ") ") . show) (scanner s)
