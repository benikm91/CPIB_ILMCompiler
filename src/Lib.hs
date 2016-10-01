module Lib
    ( someFunc
    ) where

import Data.List.Split

someFunc :: IO ()
someFunc = do 
	program <- getLine
	let res = compile program
	putStrLn res

type CompileError = String

data Bracket = OPEN | CLOSE deriving (Show)

data Token = WHILE | SEMICOLON | IDENTIFIER String | B_LITERAL Bool | BRACKET Bracket | EQUALSIGN deriving (Show)

isNoise :: String -> Bool
isNoise " " = True
isNoise "" = True
isNoise _ = False

getTokens :: String -> [Token]
getTokens s = toToken $ filter (not . isNoise) $ split (oneOf ")( =") s

toToken :: String -> Token
toToken "while" = WHILE
toToken "(" = BRACKET OPEN
toToken ")" = BRACKET CLOSE
toToken ";" = SEMICOLON
toToken "=" = EQUALSIGN
toToken "true" = B_LITERAL True
toToken "false" = B_LITERAL False
toToken x = IDENTIFIER x

-- scanner :: String -> Either CompileError [Token]
scanner :: String -> [Token]
scanner s = map toToken (getTokenWords s)

compile :: String -> String
compile s = concat $ map (( "(" ++) .(++ ") ") . show) (scanner s)