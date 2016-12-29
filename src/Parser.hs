module Parser ( readExpr, printTree, IMLVal, IMLType, IMLFlowMode, IMLChangeMode ) where

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

data IMLOperation = Plus | Minus | Not
            deriving Show

data IMLLiteral = IMLBool Bool | IMLInt Int
            deriving Show

data IMLVal = Program IMLVal [IMLVal] [IMLVal] [IMLVal] -- Name [ParamDeclarations] [FunctionDeclarations] [Statements]
            | Ident String
            | IdentDeclaration IMLChangeMode IMLVal IMLType
            | ParamDeclaration IMLFlowMode IMLChangeMode IMLVal IMLType
            | IdentFactor IMLVal (Maybe IMLVal)
            | BoolOpr IMLVal IMLVal
            | RelOpr IMLVal IMLVal
            | AddOpr IMLVal IMLVal
            | MultOpr IMLVal IMLVal
            | MonadicOpr IMLOperation IMLVal
            | Literal IMLLiteral
            | Init
            | ExprList [IMLVal]
            | Message String
            | FunctionDeclaration IMLVal [IMLVal] [IMLVal] -- Name [Parameters] [Statements]
            | FunctionCall IMLVal [IMLVal] -- Name [Parameters]
            | If IMLVal [IMLVal] [IMLVal] -- Condition [If Statements] [Else Statement]
            | While IMLVal [IMLVal] -- Condition [Statements]
            | Assignment IMLVal IMLVal -- Name Expression
            deriving Show

-- PRINT

printTabs :: Int -> String
printTabs 0 = ""
printTabs i = "\t" ++ printTabs (i-1)

addTabs :: Int -> IMLVal -> String
addTabs i val = (printTabs i) ++ (printIml i val)

printTree :: IMLVal -> String
printTree val = printIml 0 val

printIml :: Int -> IMLVal -> String
printIml i (Program name params funcs states) = "Program" ++ (printIml i name) ++ "\n" ++ (printList 0 params) ++ "\n" ++ (printList 0 funcs) ++ "\n" ++ (printList 0 states)
printIml i (Ident name) = "(Ident "++ name ++")"
printIml i t@(IdentDeclaration changemode val imltype) = show t
printIml i (ParamDeclaration imlFlowMode imlChangeMode ident imlType) = "ParamDeclaration " ++ (show imlFlowMode) ++ (show imlChangeMode) ++ (printIml i ident) ++ (show imlType)
printIml i t@(IdentFactor _ _) = show t
printIml i t@(BoolOpr iMLVala iMLValb) = show t
printIml i t@(RelOpr iMLVala iMLValb) = show t
printIml i t@(AddOpr iMLVala iMLValb) = show t
printIml i t@(MultOpr iMLVala iMLValb) = show t
printIml i t@(MonadicOpr iMLSign iMLVal) = show t
printIml i t@(Literal iMLLiteral) = show t
printIml i t@(Init) = show t
printIml i t@(ExprList iMLVals) = show t
printIml i t@(Message string) = show t
printIml i (FunctionDeclaration name params states) = "FunctionDeclaration " ++ (printIml i name) ++ "\n" ++ (printList (i) params) ++ "\n" ++ (printList (i) states)
printIml i (FunctionCall name params) = "FunctionCall " ++ (printIml i name) ++ "\n" ++ (printList i params)
printIml i (If condition ifStates elseStates) = "If " ++ (printIml i condition) ++ "\n" ++ (printList i ifStates) ++ "\n" ++ (printList i elseStates)
printIml i (While condition states) = "While " ++ (printIml i condition) ++ "\n" ++ (printList i states)

printList :: Int -> [IMLVal] -> String
printList i vals = (printTabs i) ++ "[\n" ++ (concat (intersperse ",\n" $ map (addTabs (i+1)) vals)) ++ "\n" ++ (printTabs i) ++ "]"

-- END PRINT

braces :: Parser a -> Parser a
braces  = between (string "{") (string "}")
brackets :: Parser a -> Parser a
brackets  = between (string "(") (string ")")

readExpr :: String -> IMLVal
readExpr input = case parse parseProgram "Hambbe" input of
    Left err -> Message $ "fuck you: " ++ show err
    Right val -> val

spaces :: Parser ()
spaces = skipMany space

spaces1 :: Parser ()
spaces1 = skipMany1 space

identStartChars :: String
identStartChars = ['a'..'z']++['A'..'Z']++"_"

identChars :: String
identChars = identStartChars++['0'..'9']

parseProgram :: Parser IMLVal
parseProgram = parseProgram1

parseProgram1 :: Parser IMLVal
parseProgram1 = do
    string "prog"
    spaces
    name <- parseIdent
    params <- option [] parseParamList
    spaces
    -- TODO use braces here :)
    char '{'
    functions <- parseFunctionList
    statements <- parseStatementList
    char '}'
    return $ Program name params functions statements

parseFunctionList :: Parser [IMLVal]
parseFunctionList = do
    functions <- many parseFunction
    return functions

parseStatementList :: Parser [IMLVal]
parseStatementList =  do
    spaces
    statements <- many parseStatement
    spaces
    return statements

parseStatement = 
        try parseBraketStatement
    <|> try parseIf
    <|> try parseWhile
    <|> try parseFor
    <|> try parseFunctionCall
    <|> try parseIdentDeclaration 
    -- <|> try parseBecomes / Assignment
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
    params <- brackets (parseArgument `sepBy` (string ","))
    spaces
    char ';'
    return $ FunctionCall identName params

parseArgument :: Parser IMLVal
parseArgument = do 
    spaces
    name <- parseIdent
    return name
    -- <|> parseLiteral TODO

parseIf :: Parser IMLVal
parseIf = do
    spaces
    string "if"
    spaces
    condition <- brackets parseIdent
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
    condition <- brackets parseIdent
    spaces
    statements <- braces parseStatementList
    spaces
    return $ While condition statements

parseFor :: Parser IMLVal
parseFor = do
    spaces
    string "for"
    return $ Message "TODO"

parseAssignment :: Parser IMLVal
parseAssignment = do
    spaces
    identName <- parseIdent
    spaces
    string ":="
    spaces
    expression <- parseExpr
    return $ Assignment identName expression


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

parseParamList :: Parser [IMLVal]
parseParamList = do
    params <- brackets (parseParam `sepBy` (string ","))
    return params

parseParam :: Parser IMLVal
parseParam = do
    spaces
    flowMode <- parseFlowMode
    spaces
    changeMode <- parseChangeMode
    spaces
    (identName, identType) <- parseTypedIdent
    return $ ParamDeclaration flowMode changeMode identName identType

parseIdentDeclaration :: Parser IMLVal
parseIdentDeclaration = do 
    spaces
    changeMode <- parseChangeMode
    (identName, identType) <- parseTypedIdent
    spaces
    char ';'
    return $ IdentDeclaration changeMode identName identType

parseFlowMode :: Parser IMLFlowMode
parseFlowMode = option InOut $ 
        parseString "in" In
    <|> parseString "out" Out
    <|> parseString "InOut" InOut

parseChangeMode :: Parser IMLChangeMode
parseChangeMode = try parseVal
    <|> parseVar
    <|> option Mutable parseConst

parseVal :: Parser IMLChangeMode
parseVal = parseString "val" Const

parseVar :: Parser IMLChangeMode
parseVar = parseString "var" Mutable

parseConst :: Parser IMLChangeMode
parseConst = parseString "const" Const

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

parseExpr :: Parser IMLVal
parseExpr = do 
    return $ Message "TODO"

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
    spaces
    exprList <- (brackets $ option [] parseExprListInner)
    return $ ExprList exprList

parseExprListInner :: Parser [IMLVal]
parseExprListInner = do
    expressions <- parseExpr `sepBy` (string ",")
    return expressions

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

