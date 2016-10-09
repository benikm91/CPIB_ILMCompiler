module Lib
    ( someFunc
    ) where

import Data.Char

type Token = (Terminal, Maybe Attribute)
data Terminal 
  =
  -- brackets
  LBRACKET
  | RBRACKET
  -- signs
  | NOT
  | SEMICOLON 
  | RELOPR
  | BOOLOPR
  -- Keywords
  | WHILE
  | IF
  | THEN
  | ELSE
  -- Others
  | SKIP
  | BECOMES
  | SENTINEL
  | ALITERAL
  | IDENT deriving (Show)

data Attribute
  = ALitAttr Int
  | BLitAttr Bool
  | IdentAttr String
  | ROprAttr RelOprType
  | BracketAttr BracketType
  | BOprAttr BoolOprType deriving (Show)

data RelOprType = Less | LessEq | Greater | GreaterEq | Eq  deriving (Show)
data BoolOprType = And | Or | CAnd | COr deriving(Show)
data BracketType = Round | Curly | Square deriving (Show)

isRelOp :: Char -> Bool
isRelOp '<' = True
isRelOp '>' = True
isRelOp _   = False

isBoolOp :: Char -> Bool
isBoolOp '&' = True
isBoolOp '|' = True
isBoolOp _   = False

isBracket :: Char -> Bool
isBracket '(' = True
isBracket ')' = True
isBracket _   = False

someFunc :: IO ()
someFunc = do 
	program <- getLine
	let res = compile program
	putStrLn res

compile :: String -> String
compile s = concat $ map (( "(" ++) .(++ ") ") . show) (scanner s)

scanner :: String -> [Token]
scanner cs = initState (cs ++ " ", [])

-- initState is the main state of the interpreter. It handels simple cases like 
initState :: (String, [Token]) -> [Token]
initState (c       : cs, accu)
  | isRelOp   c = initState $ relOprState (c : cs, accu)
  | isAlpha   c = initState $ identifierState (cs, [c], accu)
  | isDigit   c = initState $ numberLiteralState (cs, digitToInt c, accu)
  | isBoolOp  c = initState $ boolOprState(c : cs, accu)
  | isBracket c = initState $ bracketState (c : cs, accu)
  | isSpace   c = initState (cs, accu)
  | otherwise = error ("Lexical error: " ++ [c])
initState ([], accu) = reverse ((SENTINEL, Nothing) : accu)

boolOprState :: (String, [Token]) -> (String, [Token])
boolOprState ('&':'&' : cs, accu) = (cs, (BOOLOPR, Just $ BOprAttr  And) : accu)
boolOprState ('&':'?' : cs, accu) = (cs, (BOOLOPR, Just $ BOprAttr CAnd) : accu)
boolOprState ('|':'|' : cs, accu) = (cs, (BOOLOPR, Just $ BOprAttr  Or) : accu)
boolOprState ('|':'?' : cs, accu) = (cs, (BOOLOPR, Just $ BOprAttr COr) : accu)

relOprState :: (String, [Token]) -> (String, [Token])
relOprState ('>':'=' : cs, accu) = (cs, (RELOPR, Just $ ROprAttr GreaterEq) : accu)
relOprState ('>'     : cs, accu) = (cs, (RELOPR, Just $ ROprAttr Greater) : accu)
relOprState ('<':'=' : cs, accu) = (cs, (RELOPR, Just $ ROprAttr LessEq) : accu)
relOprState ('<'     : cs, accu) = (cs, (RELOPR, Just $ ROprAttr Less) : accu)

bracketState :: (String, [Token]) -> (String, [Token])
bracketState ('(' : cs, accu) = (cs, (LBRACKET, Just $ BracketAttr Round) : accu)
bracketState (')' : cs, accu) = (cs, (RBRACKET, Just $ BracketAttr Round) : accu)

getKeyword :: String -> Maybe Token
getKeyword "while" = Just $ (WHILE, Nothing)
getKeyword "if"    = Just $ (IF, Nothing)
getKeyword "then"  = Just $ (THEN, Nothing)
getKeyword "else"  = Just $ (ELSE, Nothing)
getKeyword "true"  = Just $ (ALITERAL, Just $ BLitAttr True)
getKeyword _ = Nothing

identifierState :: (String, String, [Token]) -> (String, [Token])
identifierState (c : cs, accu', accu)
  | isAlpha c || isDigit c = identifierState (cs, c : accu', accu)
  | otherwise = case (getKeyword word) of
    Just keyword -> (c : cs, keyword : accu)
    Nothing -> (c : cs, (IDENT, Just $ IdentAttr (word)) : accu)
    where word = reverse accu'

numberLiteralState :: (String, Int, [Token]) -> (String, [Token])
numberLiteralState (c : cs, accu', accu)
  | isDigit c = numberLiteralState (cs, 10 * accu' + digitToInt c, accu)
  | otherwise = (c : cs, (ALITERAL, Just $ ALitAttr accu') : accu)
