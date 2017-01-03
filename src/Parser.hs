module Parser ( readExpr, printTree, IMLVal(..), IMLType(..), IMLFlowMode(..), IMLChangeMode(..), IMLOperation(..), IMLLiteral(..) ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Token hiding (braces, brackets)
import System.Environment
import Data.List
import GHC.Int (Int32)

data IMLType = Int 
    | ClampInt Int Int -- min max
    | ArrayInt Int Int -- min max
    deriving Show

data IMLFlowMode = In | Out | InOut
    deriving Show

data IMLChangeMode = Const | Mutable
    deriving Show

data IMLOperation = Times | Div | Mod | Plus | Minus | Lt | Ge | Eq | Ne | Gt | Le | And | Or | Not
    deriving Show

data IMLLiteral = IMLBool Bool | IMLInt Int
    deriving Show

data IMLVal = Program IMLVal [IMLVal] [IMLVal] [IMLVal] SourcePos -- name [paramDeclarations] [functionDeclarations] [statements]
    | Ident String SourcePos -- name
    | IdentDeclaration IMLChangeMode IMLVal IMLType SourcePos -- changeMode Ident type
    | ParamDeclaration IMLFlowMode IMLChangeMode IMLVal IMLType SourcePos
    | IdentFactor IMLVal (Maybe IMLVal) SourcePos -- name _
    | IdentArray IMLVal IMLVal SourcePos -- name indexExpression
    | DyadicOpr IMLOperation IMLVal IMLVal SourcePos -- opreration expression expression
    | MonadicOpr IMLOperation IMLVal SourcePos -- opperation expression
    | Literal IMLLiteral SourcePos -- literal
    | Init -- not used
    | ExprList [IMLVal] SourcePos -- [expression]
    | Message String -- for printing error message
    | FunctionDeclaration IMLVal [IMLVal] [IMLVal] SourcePos -- name [parameters] [statements]
    | FunctionCall IMLVal [IMLVal] SourcePos -- name [parameters]
    | If IMLVal [IMLVal] [IMLVal] SourcePos -- condition [if Statements] [else Statement]
    | While IMLVal [IMLVal] SourcePos -- condition [statements]
    | For IMLVal [IMLVal] SourcePos -- condition [statements]
    | Assignment IMLVal IMLVal SourcePos-- name expression
    deriving Show

-- PRINT

printList :: Int -> [IMLVal] -> String
printList i vals = printTabs i ++ "[\n" ++ intercalate ",\n" (map (printIml (i + 1)) vals) ++ "\n" ++ printTabs i ++ "]"

printTabs :: Int -> String
printTabs i = concat ["\t" | r <- [1..i]]

printTree :: IMLVal -> String
printTree = printIml 0

printIml :: Int -> IMLVal -> String
printIml i t = printTabs i ++ printElement t
    where printElement (Program name params funcs states _) = "Program" ++ printIml i name ++ "\n" ++ printList i params ++ "\n" ++ printList i funcs ++ "\n" ++ printList i states
          printElement (Ident name _) = "(Ident "++ name ++")"
          printElement (ParamDeclaration imlFlowMode imlChangeMode ident imlType _) = "ParamDeclaration " ++ show imlFlowMode ++ " " ++ show imlChangeMode ++ " " ++ printElement ident ++ " " ++ show imlType
          printElement (Assignment name expression _) = "Assignment \n" ++ printIml (i+1) name ++ " := \n" ++ printIml (i+1) expression
          printElement (FunctionDeclaration name params states _) = "FunctionDeclaration " ++ printElement name ++ "\n" ++ printList i params ++ "\n" ++ printList i states
          printElement (FunctionCall name params _) = "FunctionCall " ++ printElement name ++ "\n" ++ printList i params
          printElement (If condition ifStates elseStates _) = "If \n" ++ printTabs i ++ "(\n" ++ printIml (i+1) condition ++ "\n" ++ printTabs i ++ ")\n" ++ printList i ifStates ++ "\n" ++ printList i elseStates
          printElement (While condition states _) = "While \n" ++ printTabs i ++ "(\n" ++ printIml (i+1) condition ++ "\n" ++ printTabs i ++ ")\n" ++ printList i states
          printElement (For condition states _) = "For \n" ++  printTabs i ++ "(\n" ++ printIml (i+1) condition ++ "\n" ++ printTabs i ++ ")\n" ++ printList i states
          printElement (DyadicOpr op term1 term2 _) = "DyadicOpr " ++ show op ++ "\n" ++ printTabs i ++ "(\n" ++ printIml (i+1) term1 ++ ",\n" ++ printIml (i+1) term2 ++ "\n" ++ printTabs i ++ ")"
          printElement (IdentFactor name _ _) = "IdentFactor " ++ printIml 0 name;
          printElement (Literal literal _) = "Literal " ++ show literal
          printElement (IdentDeclaration changeMode name imlType _) = "IdentDeclaration " ++ show changeMode ++ " " ++ printIml 0 name ++ " " ++ show imlType
          printElement (IdentArray name indexExpression _) = "IdentArray " ++ printElement name ++ "\n" ++ printIml (i+1) indexExpression
          printElement t = show t

-- END PRINT

braces, brackets :: Parser a -> Parser a
braces  = between (do string "{"; spaces) (do spaces; string "}")
brackets  = between (do string "("; spaces) (do spaces; string ")")

readExpr :: String -> IMLVal
readExpr input = case parse parseProgram "Hambbe" input of
    Left err -> Message $ "fuck you: " ++ show err
    Right val -> val

spaces, spaces1 :: Parser ()
spaces = try skipComment <|> skipMany space
spaces1 = skipMany1 space
--whiteSpaces = try skipComment <|> skipMany space

skipComment :: Parser ()
skipComment = do
    skipMany space
    string "//"
    many $ noneOf "\n"
    newline
    spaces
    return ()

identStartChars, identChars :: String
identStartChars = ['a'..'z']++['A'..'Z']++"_"
identChars = identStartChars++['0'..'9']

parseProgram :: Parser IMLVal
parseProgram = do
    pos <- getPosition
    string "prog"
    spaces
    name <- parseIdent
    spaces
    params <- option [] parseParamList
    spaces
    -- TODO use braces here :)
    char '{'
    functions <- parseFunctionList
    statements <- parseStatementList
    spaces
    char '}'
    return $ Program name params functions statements pos

parseFunctionList, parseStatementList, parseParamList :: Parser [IMLVal]
parseFunctionList  = many parseFunction
parseStatementList = many parseStatement
parseParamList = brackets (parseParam `sepBy` string ",")

-- Statement

parseStatement :: Parser IMLVal
parseStatement = 
    parseIf
    <|> parseWhile
    <|> parseFor
    <|> try parseBraketStatement
    <|> try parseIdentDeclaration
    <|> try parseAssignment
    <|> parseFunctionCall
    <?> "Could not parse statement"

parseBraketStatement :: Parser IMLVal
parseBraketStatement = do 
    spaces
    statement <- brackets parseStatement
    return statement

parseFunctionCall :: Parser IMLVal
parseFunctionCall = do
    (pos, identName, params) <- try $ do
        spaces
        pos <- getPosition
        identName <- parseIdent
        spaces
        params <- brackets (parseArgument `sepBy` string ",")
        return (pos, identName, params)
    spaces
    char ';'
    return $ FunctionCall identName params pos

parseArgument :: Parser IMLVal
parseArgument = do 
    spaces
    name <- parseExpr
    return name
    
parseIf :: Parser IMLVal
parseIf = do
    pos <- try $ do
        spaces
        pos <- getPosition
        string "if"
        return pos
    spaces
    condition <- brackets parseExpr
    spaces
    ifStatements <- braces parseStatementList
    spaces
    elseStatements <- option [] parseElse
    return $ If condition ifStatements elseStatements pos

parseElse :: Parser [IMLVal]
parseElse = do
    spaces
    string "else"
    spaces
    statements <- braces parseStatementList
    return statements

parseWhile :: Parser IMLVal
parseWhile = do
    pos <- try $ do
        spaces
        pos <- getPosition
        string "while"
        return pos
    spaces
    condition <- brackets parseExpr
    spaces
    statements <- braces parseStatementList
    --spaces
    return $ While condition statements pos

parseFor :: Parser IMLVal
parseFor = do
    pos <- try $ do
        spaces
        pos <- getPosition
        string "for"
        return pos
    spaces
    condition <- brackets parseExpr
    spaces
    statements <- braces parseStatementList
    return $ For condition statements pos

parseAssignment :: Parser IMLVal
parseAssignment = do
    --(pos, identName) <- try $ do 
    spaces
    pos <- getPosition
    identName <- parseIdentOrArrayIdent
    spaces
    string ":="
    spaces
        --return (pos, identName)
    expression <- parseExpr
    spaces
    char ';'
    return $ Assignment identName expression pos

parseIdentOrArrayIdent :: Parser IMLVal
parseIdentOrArrayIdent = try parseArrayIdent <|> parseIdent

parseArrayIdent :: Parser IMLVal
parseArrayIdent = do 
    spaces
    pos <- getPosition
    ident <- parseIdent
    spaces
    string "["
    spaces
    index <- parseExpr
    spaces
    string "]"
    return $ IdentArray ident index pos

parseIdentDeclaration :: Parser IMLVal
parseIdentDeclaration = do
    spaces
    pos <- getPosition
    changeMode <- parseChangeMode
    (identName, identType) <- parseTypedIdent
    spaces
    char ';'
    return $ IdentDeclaration changeMode identName identType pos

-- Function

parseFunction :: Parser IMLVal
parseFunction = do
    pos <- try $ do
        spaces
        pos <- getPosition
        try $ string "def";
        return pos
    spaces
    identName <- parseIdent
    spaces
    params <- parseParamList
    spaces
    statements <- braces parseStatementList
    return $ FunctionDeclaration identName params statements pos

parseParam :: Parser IMLVal
parseParam = do
    spaces
    pos <- getPosition
    flowMode <- parseFlowMode
    spaces
    changeMode <- parseChangeMode
    spaces
    (identName, identType) <- parseTypedIdent
    return $ ParamDeclaration flowMode changeMode identName identType pos

parseFlowMode :: Parser IMLFlowMode
parseFlowMode = 
        try (parseString "inOut" InOut)
    <|> try (parseString "in"    In)
    <|> try (parseString "out"   Out)

-- ChangeMode

parseChangeMode :: Parser IMLChangeMode
parseChangeMode = 
        try parseVal
    <|> try parseVar
    <|> (option Mutable (try parseConst))

parseVal, parseVar, parseConst :: Parser IMLChangeMode
parseVal = parseString "val" Const
parseVar = parseString "var" Mutable
parseConst = parseString "const" Const

-- Identifier

parseTypedIdent :: Parser (IMLVal, IMLType)
parseTypedIdent = do
    spaces
    identName <- parseIdent
    spaces
    char ':'
    spaces
    identType <- parseType
    return (identName, identType)

parseType :: Parser IMLType
parseType = try praseIntClamp <|> try praseIntArray <|> parseString "int" Int

praseIntClamp :: Parser IMLType
praseIntClamp = do 
        spaces
        string "int"
        spaces
        string "("
        spaces
        oprA <- option id (parseChar '-' negate)
        spaces
        a <- optionMaybe (many1 digit)
        spaces
        string ".."
        spaces
        oprB <- option id (parseChar '-' negate)
        spaces
        b <- optionMaybe (many1 digit)
        spaces
        string ")"
        return $ ClampInt (oprA (maybe (fromIntegral (minBound :: Int32) :: Int) (\x -> read x :: Int) a)) (oprB (maybe (fromIntegral (maxBound :: Int32) :: Int) (\x -> read x :: Int) b))

praseIntArray :: Parser IMLType
praseIntArray = do 
        spaces
        string "int"
        spaces
        string "["
        spaces
        oprA <- option id (parseChar '-' negate)
        spaces
        a <- many1 digit
        spaces
        string ".."
        spaces
        oprB <- option id (parseChar '-' negate)
        spaces
        b <- many1 digit
        spaces
        string "]"
        return $ ArrayInt (oprA (read a :: Int)) (oprB (read b :: Int))

parseIdent :: Parser IMLVal
parseIdent = do
                pos <- getPosition
                head <- oneOf identStartChars
                tail <- many $ oneOf identChars
                return $ Ident (head : tail) pos

-- EXPR

parseExpr :: Parser IMLVal
parseExpr = try parseBoolExpr
    <|> try parseTerm1

parseTerm1 :: Parser IMLVal
parseTerm1 = try parseRelExpr
    <|> try parseTerm2

parseTerm2 :: Parser IMLVal
parseTerm2 = try parseAddExpr
    <|> try parseTerm3

parseTerm3 :: Parser IMLVal
parseTerm3 = try parseMulExpr
    <|> try parseFactor

-- BOOLEXPR

parseBoolExpr :: Parser IMLVal
parseBoolExpr = do
    spaces
    pos <- getPosition
    firstTerm <- parseTerm1
    spaces
    opr <- parseBoolOpr
    spaces
    secondTerm <- parseExpr
    return $ DyadicOpr opr firstTerm secondTerm pos

parseBoolOpr :: Parser IMLOperation
parseBoolOpr = try parseAnd
    <|> try parseOr

parseAnd, parseOr :: Parser IMLOperation
parseAnd = parseString "&?" And
parseOr = parseString "|?" Or

-- RELEXPR

parseRelExpr :: Parser IMLVal
parseRelExpr = do
    spaces
    pos <- getPosition
    firstTerm <- parseTerm2
    spaces
    opr <- parseRelOpr
    spaces
    secondTerm <- parseTerm2
    return $ DyadicOpr opr firstTerm secondTerm pos

parseRelOpr :: Parser IMLOperation
parseRelOpr = try parseEq
    <|> try parseNe
    <|> try parseLe
    <|> try parseGe
    <|> try parseLt
    <|> try parseGt

parseEq, parseNe, parseLt, parseGt, parseLe, parseGe :: Parser IMLOperation
parseEq = parseString "=" Eq
parseNe = parseString "/=" Ne <|> parseString "≠" Ne 
parseLt = parseString "<" Lt
parseGt = parseString ">" Gt
parseLe = parseString "<=" Le <|> parseString "≤" Le 
parseGe = parseString ">=" Ge <|> parseString "≥" Ge 

-- ADDEXPR

parseAddExpr :: Parser IMLVal
parseAddExpr = do
    spaces
    pos <- getPosition
    firstTerm <- parseTerm3
    spaces
    opr <- parseAddOpr
    spaces
    secondTerm <- parseTerm2
    return $ DyadicOpr opr firstTerm secondTerm pos

parseAddOpr :: Parser IMLOperation
parseAddOpr = try parsePlus
    <|> try parseMinus

parsePlus, parseMinus :: Parser IMLOperation
parsePlus = parseString "+" Plus
parseMinus = parseString "-" Minus

-- MULEXPR

parseMulExpr :: Parser IMLVal
parseMulExpr = do
    spaces
    pos <- getPosition
    firstTerm <- try parseFactor
    spaces
    opr <- try parseMulOpr
    spaces
    secondTerm <- try parseTerm3
    return $ DyadicOpr opr firstTerm secondTerm pos

parseMulOpr :: Parser IMLOperation
parseMulOpr = try parseTimes

parseTimes :: Parser IMLOperation
parseTimes = parseString "*" Times <|> parseString "/" Div <|> parseString "%" Mod

-- FACTOR

parseFactor :: Parser IMLVal
parseFactor = 
        try parseTrue
    <|> try parseFalse
    <|> try parseNumber
    <|> try parseArrayIdentFactor
    <|> try parseIdentFactor
    <|> try parseMonadicOpr
    <|> try (brackets parseExpr)

parseMonadicOpr :: Parser IMLVal
parseMonadicOpr = do
    spaces
    pos <- getPosition
    opr <- try parseOpr
    spaces
    factor <- try parseFactor
    return $ MonadicOpr opr factor pos

parseIdentFactor :: Parser IMLVal
parseIdentFactor = do
    spaces
    pos <- getPosition
    ident <- try parseIdent
    spaces
    identAddition <- try $ optionMaybe (choice [ parseInit, parseExprList ])
    return $ IdentFactor ident identAddition pos

parseArrayIdentFactor :: Parser IMLVal
parseArrayIdentFactor = do
    spaces
    pos <- getPosition
    ident <- try parseIdent
    spaces
    string "["
    spaces
    i <- parseExpr
    spaces
    string "]"
    return $ IdentArray ident i pos

-- TODO Perhaps parseTrue, parseFalse, parseNumber as where functions :) not sure
parseTrue :: Parser IMLVal
parseTrue = do 
    pos <- getPosition
    string "true"
    return (Literal (IMLBool True) pos)

parseFalse :: Parser IMLVal
parseFalse = do
    pos <- getPosition
    string "false" 
    return (Literal (IMLBool False) pos)

parseNumber :: Parser IMLVal
parseNumber = do
    pos <- getPosition
    literal <- try $ read <$> many1 digit
    return $ Literal (IMLInt literal) pos

parseInit :: Parser IMLVal
parseInit = parseString "init" Init

parseExprList :: Parser IMLVal
parseExprList  = do
    pos <- getPosition
    exprList <- (brackets $ option [] parseExprListInner)
    return $ ExprList exprList pos

parseExprListInner :: Parser [IMLVal]
parseExprListInner = parseExpr `sepBy` string ","

parseOpr :: Parser IMLOperation
parseOpr =
        parseChar '!' Not
    <|> parseChar '+' Plus
    <|> parseChar '-' Minus

parseChar :: Char -> a -> Parser a
parseChar c r = do 
    char c
    return r

parseString :: String -> a -> Parser a
parseString s r = do
    string s
    return r

