module Scanner ( scanner ) where

import Data.Char
import Token

-- Help Function for Char.
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
boolOprState ('&':'&' : cs, accu) = (cs, (BOOLOPR, Just $ BoolOprAttr  And) : accu)
boolOprState ('&':'?' : cs, accu) = (cs, (BOOLOPR, Just $ BoolOprAttr CAnd) : accu)
boolOprState ('|':'|' : cs, accu) = (cs, (BOOLOPR, Just $ BoolOprAttr  Or) : accu)
boolOprState ('|':'?' : cs, accu) = (cs, (BOOLOPR, Just $ BoolOprAttr COr) : accu)

relOprState :: (String, [Token]) -> (String, [Token])
relOprState ('>':'=' : cs, accu) = (cs, (RELOPR, Just $ RelOprAttr GreaterEq) : accu)
relOprState ('>'     : cs, accu) = (cs, (RELOPR, Just $ RelOprAttr Greater) : accu)
relOprState ('<':'=' : cs, accu) = (cs, (RELOPR, Just $ RelOprAttr LessEq) : accu)
relOprState ('<'     : cs, accu) = (cs, (RELOPR, Just $ RelOprAttr Less) : accu)

bracketState :: (String, [Token]) -> (String, [Token])
bracketState ('(' : cs, accu) = (cs, (LBRACKET, Just $ BracketAttr Round) : accu)
bracketState (')' : cs, accu) = (cs, (RBRACKET, Just $ BracketAttr Round) : accu)

getKeyword :: String -> Maybe Token
getKeyword "while" = Just $ (WHILE, Nothing)
getKeyword "if"    = Just $ (IF, Nothing)
getKeyword "then"  = Just $ (THEN, Nothing)
getKeyword "else"  = Just $ (ELSE, Nothing)
getKeyword "true"  = Just $ (LITERAL, Just $ BoolLitAttr True)
getKeyword "false"  = Just $ (LITERAL, Just $ BoolLitAttr False)
getKeyword _       = Nothing

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
  | otherwise = (c : cs, (LITERAL, Just $ IntLitAttr accu') : accu)
