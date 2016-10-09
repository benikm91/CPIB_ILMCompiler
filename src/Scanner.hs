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

isArimOp :: Char -> Bool
isArimOp '+' = True
isArimOp '-' = True
isArimOp '*' = True
isArimOp '/' = True
isArimOp '%' = True
isArimOp _   = False


isBracket :: Char -> Bool
isBracket '(' = True
isBracket ')' = True
isBracket _   = False

isSpecialSign :: Char -> Bool
isSpecialSign ':' = True
isSpecialSign ',' = True
isSpecialSign ';' = True
isSpecialSign _   = False


scanner :: String -> [Token]
scanner cs = initState (cs ++ " ", [])

-- initState is the main state of the interpreter. It handels simple cases like 
initState :: (String, [Token]) -> [Token]
initState (c : cs, accu)
  | isArimOp      c = initState $ arimOprState (c : cs, accu)
  | isSpecialSign c = initState $ specialSignState (c : cs, accu)
  | isRelOp       c = initState $ relOprState (c : cs, accu)
  | isLetter      c = initState $ identifierState (cs, [c], accu)
  | isDigit       c = initState $ numberLiteralState (cs, digitToInt c, accu)
  | isBoolOp      c = initState $ boolOprState(c : cs, accu)
  | isBracket     c = initState $ bracketState (c : cs, accu)
  | isSpace       c = initState (cs, accu)
  | otherwise       = error ("Lexical error: " ++ [c])
initState ([], accu) = reverse ((SENTINEL, Nothing) : accu)

arimOprState :: (String, [Token]) -> (String, [Token])
arimOprState ('+' : cs, accu) = (cs, (PLUS, Nothing) : accu)
arimOprState ('-' : cs, accu) = (cs, (MINUS, Nothing) : accu)
arimOprState ('*' : cs, accu) = (cs, (TIMES, Nothing) : accu)
arimOprState ('/' : cs, accu) = (cs, (DIV, Nothing) : accu)
arimOprState ('%' : cs, accu) = (cs, (MOD, Nothing) : accu)

specialSignState :: (String, [Token]) -> (String, [Token])
specialSignState (':':'=' : cs, accu) = (cs, (ASSIGNMENT, Nothing) : accu)
specialSignState (':'     : cs, accu) = (cs, (COLON, Nothing) : accu)
specialSignState (','     : cs, accu) = (cs, (COMMA, Nothing) : accu)
specialSignState (';'     : cs, accu) = (cs, (SEMICOLON, Nothing) : accu)

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
getKeyword "while"      = Just $ (WHILE, Nothing)
getKeyword "endwhile"   = Just $ (END_WHILE, Nothing)
getKeyword "if"         = Just $ (IF, Nothing)
getKeyword "then"       = Just $ (THEN, Nothing)
getKeyword "else"       = Just $ (ELSE, Nothing)
getKeyword "true"       = Just $ (LITERAL, Just $ BoolLitAttr True)
getKeyword "false"      = Just $ (LITERAL, Just $ BoolLitAttr False)
getKeyword "program"    = Just $ (PROGRAM, Nothing)
getKeyword "endprogram" = Just $ (END_PROGRAM, Nothing)
getKeyword "in"         = Just $ (IN, Nothing)
getKeyword "out"        = Just $ (OUT, Nothing)
getKeyword "const"      = Just $ (CONST, Nothing)
getKeyword "int64"      = Just $ (TYPE, Just $ TypeAttr $ Integer 64)
getKeyword "global"     = Just $ (GLOBAL, Nothing)
getKeyword "proc"       = Just $ (PROCEDURE, Nothing)
getKeyword "endproc"    = Just $ (END_PROCEDURE, Nothing)
getKeyword "copy"       = Just $ (COPY, Nothing)
getKeyword "ref"        = Just $ (REF, Nothing)
getKeyword "do"         = Just $ (DO, Nothing)
getKeyword _            = Nothing

identifierState :: (String, String, [Token]) -> (String, [Token])
identifierState (c : cs, accu', accu)
  | isLetter c || isDigit c = identifierState (cs, c : accu', accu)
  | otherwise = case (getKeyword word) of
    Just keyword -> (c : cs, keyword : accu)
    Nothing -> (c : cs, (IDENT, Just $ IdentAttr (word)) : accu)
    where word = reverse accu'

numberLiteralState :: (String, Int, [Token]) -> (String, [Token])
numberLiteralState (c : cs, accu', accu)
  | isDigit c = numberLiteralState (cs, 10 * accu' + digitToInt c, accu)
  | otherwise = (c : cs, (LITERAL, Just $ IntLitAttr accu') : accu)
