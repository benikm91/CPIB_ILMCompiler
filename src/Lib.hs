module Lib
    ( someFunc
    ) where

import Data.Char

type Token = (Terminal, Maybe Attribute)
data Terminal 
  = IDENT
  | NOT
  | LPAREN
  | RPAREN
  | BECOMES
  | SEMICOLON 
  | SKIP
  | IF
  | THEN
  | ELSE
  | RELOPR
  | ALITERAL
  | SENTINEL deriving (Show)

data Attribute
  = ALitAttrib Int
  | BLitAttrib Bool
  | IdentAttrib String
  | ROprAttrib RelOprType deriving (Show)

data RelOprType = Less | LessEq | Greater | GreaterEq | Eq  deriving (Show)

someFunc :: IO ()
someFunc = do 
	program <- getLine
	let res = compile program
	putStrLn res

compile :: String -> String
compile s = concat $ map (( "(" ++) .(++ ") ") . show) (scanner s)

scanner :: String -> [Token]
scanner cs = initState (cs ++ " ", [])

initState :: (String, [Token]) -> [Token]
initState ('<':'=' : cs, accu) = initState (cs, (RELOPR, Just (ROprAttrib LessEq)) : accu)
initState ('<'     : cs, accu) = initState (cs, (RELOPR, Just (ROprAttrib Less)) : accu)
initState (c       : cs, accu)
  | isAlpha c = initState (identifierState (cs, [c], accu))
  | isDigit c = initState (numberLiteralState (cs, digitToInt c, accu))
  | isSpace c = initState (cs, accu)
  | otherwise = error ("Lexical error: " ++ [c])
initState ([], accu) = reverse ((SENTINEL, Nothing) : accu)

identifierState :: (String, String, [Token]) -> (String, [Token])
identifierState (c : cs, accu', accu)
  | isAlpha c || isDigit c = identifierState (cs, c : accu', accu)
  | otherwise = (c : cs, (IDENT, Just (IdentAttrib (reverse accu'))) : accu)

numberLiteralState :: (String, Int, [Token]) -> (String, [Token])
numberLiteralState (c : cs, accu', accu)
  | isDigit c = numberLiteralState (cs, 10 * accu' + digitToInt c, accu)
  | otherwise = (c : cs, (ALITERAL, Just (ALitAttrib accu')) : accu)
