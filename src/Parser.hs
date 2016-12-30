module Parser ( readExpr, printTree, IMLVal(..), IMLType, IMLFlowMode(..), IMLChangeMode, IMLOperation, IMLLiteral ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Token hiding (braces, brackets)
import System.Environment
import Data.List

data IMLType = Int
            deriving Show

data IMLFlowMode = In | Out | InOut
            deriving Show

data IMLChangeMode = Const | Mutable
            deriving Show

data IMLOperation = Times
            | Plus | Minus
            | Lt | Ge | Eq | Ne | Gt | Le
            | And | Or | Not
            deriving Show

data IMLLiteral = IMLBool Bool | IMLInt Int
            deriving Show

data IMLVal = Program IMLVal [IMLVal] [IMLVal] [IMLVal] -- Name [ParamDeclarations] [FunctionDeclarations] [Statements]
            | Ident String
            | IdentDeclaration IMLChangeMode IMLVal IMLType
            | ParamDeclaration IMLFlowMode IMLChangeMode IMLVal IMLType
            | IdentFactor IMLVal (Maybe IMLVal)
            | DyadicOpr IMLOperation IMLVal IMLVal
            | MonadicOpr IMLOperation IMLVal
            | Literal IMLLiteral
            | Init
            | ExprList [IMLVal]
            | Message String
            | FunctionDeclaration IMLVal [IMLVal] [IMLVal] -- Name [Parameters] [Statements]
            | FunctionCall IMLVal [IMLVal] -- Name [Parameters]
            | If IMLVal [IMLVal] [IMLVal] -- Condition [If Statements] [Else Statement]
            | While IMLVal [IMLVal] -- Condition [Statements]
            | For IMLVal [IMLVal] -- Condition [Statements]
            | Assignment IMLVal IMLVal -- Name Expression
            deriving Show
            
-- PRINT

printList :: Int -> [IMLVal] -> String
printList i vals = printTabs i ++ "[\n" ++ intercalate ",\n" (map (printIml (i + 1)) vals) ++ "\n" ++ printTabs i ++ "]"

printTabs :: Int -> String
printTabs i = concat ["\t" | r <- [0..i]]

printTree :: IMLVal -> String
printTree = printIml 0

printIml :: Int -> IMLVal -> String
printIml i t = printTabs i ++ printElement t
    where printElement (Program name params funcs states) = "Program" ++ printIml i name ++ "\n" ++ printList i params ++ "\n" ++ printList i funcs ++ "\n" ++ printList i states
          printElement (Ident name) = "(Ident "++ name ++")"
          printElement (ParamDeclaration imlFlowMode imlChangeMode ident imlType) = "ParamDeclaration " ++ show imlFlowMode ++ " " ++ show imlChangeMode ++ " " ++ printElement ident ++ " " ++ show imlType
          printElement (Assignment name expression) = "Assignment" ++ show name ++ " := " ++ show expression
          printElement (FunctionDeclaration name params states) = "FunctionDeclaration " ++ printIml i name ++ "\n" ++ printList i params ++ "\n" ++ printList i states
          printElement (FunctionCall name params) = "FunctionCall " ++ printIml i name ++ "\n" ++ printList i params
          printElement (If condition ifStates elseStates) = "If \n" ++ printTabs i ++ "(\n" ++ printIml (i+1) condition ++ "\n" ++ printTabs i ++ ")\n" ++ printList i ifStates ++ "\n" ++ printList i elseStates
          printElement (While condition states) = "While \n" ++ printTabs i ++ "(\n" ++ printIml (i+1) condition ++ "\n" ++ printTabs i ++ ")\n" ++ printList i states
          printElement (DyadicOpr op term1 term2) = "DyadicOpr " ++ show op ++ "\n" ++ printTabs i ++ "(\n" ++ printIml (i+1) term1 ++ ",\n" ++ printIml (i+1) term2 ++ "\n" ++ printTabs i ++ ")"
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
spaces = skipMany space
spaces1 = skipMany1 space

identStartChars, identChars :: String
identStartChars = ['a'..'z']++['A'..'Z']++"_"
identChars = identStartChars++['0'..'9']

parseProgram :: Parser IMLVal
parseProgram = do
    string "prog"
    spaces
    name <- parseIdent
    params <- option [] parseParamList
    spaces
    -- TODO use braces here :)
    char '{'
    functions <- parseFunctionList
    statements <- parseStatementList
    spaces
    char '}'
    return $ Program name params functions statements

parseFunctionList, parseStatementList, parseParamList :: Parser [IMLVal]
parseFunctionList  = many $ try parseFunction
parseStatementList = many $ try parseStatement
parseParamList = brackets (parseParam `sepBy` string ",")

-- Statement

parseStatement :: Parser IMLVal
parseStatement = 
        try parseBraketStatement
    <|> try parseIf
    <|> try parseWhile
    <|> try parseFor
    <|> try parseFunctionCall
    <|> try parseIdentDeclaration 
    <|> try parseAssignment
    <?> "Could not parse statement"

parseBraketStatement :: Parser IMLVal
parseBraketStatement = do 
    spaces
    statement <- brackets parseStatement
    spaces
    return statement

parseFunctionCall :: Parser IMLVal
parseFunctionCall = do
    spaces
    identName <- parseIdent
    spaces
    params <- brackets (parseArgument `sepBy` string ",")
    spaces
    char ';'
    return $ FunctionCall identName params

parseArgument :: Parser IMLVal
parseArgument = do 
    spaces
    name <- parseExpr
    return name
    
parseIf :: Parser IMLVal
parseIf = do
    spaces
    string "if"
    spaces
    condition <- brackets parseExpr
    spaces
    ifStatements <- braces parseStatementList
    spaces
    elseStatements <- option [] parseElse
    return $ If condition ifStatements elseStatements

parseElse :: Parser [IMLVal]
parseElse = do
    spaces
    string "else"
    spaces
    statements <- braces parseStatementList
    return statements

parseWhile :: Parser IMLVal
parseWhile = do
    spaces
    string "while"
    spaces
    condition <- brackets parseExpr
    spaces
    statements <- braces parseStatementList
    spaces
    return $ While condition statements

parseFor :: Parser IMLVal
parseFor = do
    spaces
    string "for"
    spaces
    condition <- brackets parseExpr
    spaces
    statements <- braces parseStatementList
    return $ For condition statements

parseAssignment :: Parser IMLVal
parseAssignment = do
    spaces
    identName <- parseIdent
    spaces
    string ":="
    spaces
    expression <- parseExpr
    spaces
    char ';'
    return $ Assignment identName expression

parseIdentDeclaration :: Parser IMLVal
parseIdentDeclaration = do 
    spaces
    changeMode <- parseChangeMode
    (identName, identType) <- parseTypedIdent
    spaces
    char ';'
    return $ IdentDeclaration changeMode identName identType

-- Function

parseFunction :: Parser IMLVal
parseFunction = do
    spaces
    string "def"
    spaces
    identName <- parseIdent
    spaces
    params <- parseParamList
    spaces
    statements <- braces parseStatementList
    spaces
    return $ FunctionDeclaration identName params statements

parseParam :: Parser IMLVal
parseParam = do
    spaces
    flowMode <- parseFlowMode
    spaces
    changeMode <- parseChangeMode
    spaces
    (identName, identType) <- parseTypedIdent
    return $ ParamDeclaration flowMode changeMode identName identType

parseFlowMode :: Parser IMLFlowMode
parseFlowMode = 
        try (parseString "inOut" InOut)
    <|> try (parseString "in"    In)
    <|> try (parseString "out"   Out)

-- ChangeMode

parseChangeMode :: Parser IMLChangeMode
parseChangeMode = 
    try parseVal
    <|> parseVar
    <|> option Mutable parseConst

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
parseType = parseString "int" Int

parseIdent :: Parser IMLVal
parseIdent = do
                head <- oneOf identStartChars
                tail <- many $ oneOf identChars
                return $ Ident (head : tail)

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
    firstTerm <- parseTerm1
    spaces
    opr <- parseBoolOpr
    spaces
    secondTerm <- parseExpr
    return $ DyadicOpr opr firstTerm secondTerm

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
    firstTerm <- parseTerm2
    spaces
    opr <- parseRelOpr
    spaces
    secondTerm <- parseTerm2
    return $ DyadicOpr opr firstTerm secondTerm

parseRelOpr :: Parser IMLOperation
parseRelOpr = try parseEq
    <|> try parseNe
    <|> try parseLt
    <|> try parseGt
    <|> try parseLe
    <|> try parseGe

parseEq, parseNe, parseLt, parseGt, parseLe, parseGe :: Parser IMLOperation
parseEq = parseString "=" Eq
parseNe = parseString "/=" Ne
parseLt = parseString "<" Lt
parseGt = parseString ">" Gt
parseLe = parseString "<=" Le
parseGe = parseString ">=" Ge

-- ADDEXPR

parseAddExpr :: Parser IMLVal
parseAddExpr = do
    spaces
    firstTerm <- parseTerm3
    spaces
    opr <- parseAddOpr
    spaces
    secondTerm <- parseTerm2
    return $ DyadicOpr opr firstTerm secondTerm

parseAddOpr :: Parser IMLOperation
parseAddOpr = try parseAnd
    <|> try parseOr

parsePlus, parseMinus :: Parser IMLOperation
parsePlus = parseString "+" Plus
parseMinus = parseString "-" Minus

-- MULEXPR

parseMulExpr :: Parser IMLVal
parseMulExpr = do
    spaces
    firstTerm <- try parseFactor
    spaces
    opr <- try parseMulOpr
    spaces
    secondTerm <- try parseTerm3
    return $ DyadicOpr opr firstTerm secondTerm

parseMulOpr :: Parser IMLOperation
parseMulOpr = try parseTimes

parseTimes :: Parser IMLOperation
parseTimes = parseString "*" Times

-- FACTOR

parseFactor :: Parser IMLVal
parseFactor = 
        try parseTrue
    <|> try parseFalse
    <|> try parseNumber
    <|> try parseIdentFactor
    <|> try parseMonadicOpr
    <|> try (brackets parseExpr)

parseMonadicOpr :: Parser IMLVal
parseMonadicOpr = do
    spaces
    opr <- try parseOpr
    spaces
    factor <- try parseFactor
    return $ MonadicOpr opr factor

parseIdentFactor :: Parser IMLVal
parseIdentFactor = do
    spaces
    ident <- try parseIdent
    spaces
    identAddition <- try $ optionMaybe (choice [ parseInit, parseExprList ])
    return $ IdentFactor ident identAddition

-- TODO Perhaps parseTrue, parseFalse, parseNumber as where functions :) not sure
parseTrue :: Parser IMLVal
parseTrue = parseString "true" (Literal $ IMLBool True)

parseFalse :: Parser IMLVal
parseFalse = parseString "false" (Literal $ IMLBool False)

parseNumber :: Parser IMLVal
parseNumber = do
    literal <- try $ read <$> many1 digit
    return $ Literal $ IMLInt literal

parseInit :: Parser IMLVal
parseInit = parseString "init" Init

parseExprList :: Parser IMLVal
parseExprList  = do
    exprList <- (brackets $ option [] parseExprListInner)
    return $ ExprList exprList

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
