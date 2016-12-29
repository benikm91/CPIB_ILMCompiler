module Parser ( readExpr, printTree, IMLVal, IMLType, IMLFlowMode, IMLChangeMode ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Token hiding (braces, brackets)
import System.Environment

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
            | Assignment IMLVal IMLVal -- Name Expression
            deriving Show

-- Programm
-- [ParamDeclaration In Mutable (Ident "m") Int,ParamDeclaration In Mutable (Ident "n") Int,ParamDeclaration Out Mutable (Ident "o") Int]
-- [FunctionCall (Ident "func1") [Ident "m",Ident "o1"]] [FunctionCall
-- (Ident "func2") [Ident "m",Ident "o1"]],While (Ident "o1") [FunctionCall (Ident "func1") [Ident "m",Ident "o1"]],IdentDeclaration Mutable (Ident "o2") Int,FunctionCall (Ident "func2")
-- [Ident "n",Ident "o2"]]

-- instance Show (IMLVal) where 
--     show Program name params funcs states = "Programm (Ident "++ name ++ ") ["++ show params ++"]"
--     show ParamDeclaration = 

printTree :: IMLVal -> String
printTree (Program name params funcs states) = "Program" ++ (printTree name) ++ "\n" ++ (concat $ map printTree params) ++ "\n" ++ (concat $ map printTree funcs) ++ "\n" ++ (concat $ map printTree states)
printTree (Ident name) = "(Ident "++ name ++")"
printTree t@(IdentDeclaration changemode val imltype) = show t
printTree t@(ParamDeclaration iMLFlowMode iMLChangeMode iMLVal iMLType) = show t
printTree t@(IdentFactor _ _) = show t
--printTree t@(BoolOpr iMLVala iMLValb) = show t
--printTree t@(RelOpr iMLVala iMLValb) = show t
--printTree t@(AddOpr iMLVala iMLValb) = show t
--printTree t@(MultOpr iMLVala iMLValb) = show t
printTree t@(MonadicOpr iMLSign iMLVal) = show t
printTree t@(Literal iMLLiteral) = show t
printTree t@(Init) = show t
printTree t@(ExprList iMLVals) = show t
printTree t@(Message string) = show t
printTree t@(FunctionDeclaration iMLVal iMLValsa iMLValsb) = show t -- Name [Parameters] [Statements]
printTree t@(FunctionCall iMLVal iMLVals) = show t -- Name [Parameters]
printTree t@(If iMLVal iMLValsa iMLValsb) = show t -- Condition [If Statements] [Else Statement]
printTree t@(While iMLVal iMLVals) = show t -- Condition [Statements]

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
    term <- try parseTerm1
    return try parseBoolExpr term <|> term

parseTerm1 :: Parser IMLVal
parseTerm1 = do 
    term <- parseTerm2
    return $ option term $ parseRelExpr term

parseTerm2 :: Parser IMLVal
parseTerm2 = do 
    term <- parseTerm3
    return $ option term $ parseRelExpr term

parseTerm3 :: Parser IMLVal
parseTerm3 = do 
    term <- parseFactor
    return $ option term $ parseRelExpr term

-- BOOLEXPR

parseBoolExpr :: Parser IMLVal -> Parser IMLVal
parseBoolExpr firstTerm = do
    opr <- try parseBoolOpr
    spaces
    secondTerm <- try parseExpr
    return DyadicOpr opr firstTerm secondTerm

parseBoolOpr :: Parser IMLVal
parseBoolOpr = do
        try parseAnd
    <|> try parseOr

parseAnd :: Parser IMLOperation
parseAnd = do
    spaces
    try string "&?"
    return And

parseOr :: Parser IMLOperation
parseOr = do
    spaces
    try string "|?"
    return Or

-- RELEXPR

parseRelExpr :: IMLVal -> Parser IMLVal
parseRelExpr firstTerm = do
    opr <- try parseRelOpr
    spaces
    secondTerm <- try parseTerm2
    return DyadicOpr opr firstTerm secondTerm

parseRelOpr :: Parser IMLVal
parseRelOpr = do
        try parseEq
    <|> try parseNe
    <|> try parseLt
    <|> try parseGt
    <|> try parseLe
    <|> try parseGe

parseEq :: Parser IMLOperation
parseEq = do
    spaces
    try string "="
    return Eq

parseNe :: Parser IMLOperation
parseNe = do
    spaces
    try string "/="
    return Ne

parseLt :: Parser IMLOperation
parseLt = do
    spaces
    try string "<"
    return Lt

parseGt :: Parser IMLOperation
parseGt = do
    spaces
    try string ">"
    return Gt

parseLe :: Parser IMLOperation
parseLe = do
    spaces
    try string "<="
    return Le

parseGe :: Parser IMLOperation
parseGe = do
    spaces
    try string ">="
    return Ge

-- ADDEXPR

parseAddExpr :: IMLVal -> Parser IMLVal
parseAddExpr firstTerm = do
    opr <- try parseAddOpr
    spaces
    secondTerm <- try parseTerm2
    return DyadicOpr opr firstTerm secondTerm

parseAddOpr :: Parser IMLVal
parseAddOpr = do
        try parseAnd
    <|> try parseOr

parsePlus :: Parser IMLOperation
parsePlus = do
    spaces
    try string "+"
    return Plus

parseMinus :: Parser IMLOperation
parseMinus = do
    spaces
    try string "-"
    return Minus

-- MULEXPR

parseMulExpr :: IMLVal -> Parser IMLVal
parseMulExpr firstTerm = do
    opr <- try parseMulOpr
    spaces
    secondTerm <- try parseTerm3
    return DyadicOpr opr firstTerm secondTerm

parseMulOpr :: Parser IMLVal
parseMulOpr = do
        try parseTimes

parseTimes :: Parser IMLOperation
parseTimes = do
    spaces
    try string "*"
    return Times

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
