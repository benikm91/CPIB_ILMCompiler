module Parser ( readExpr, IMLVal, IMLType, IMLFlowMode, IMLChangeMode ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Token hiding (braces, brackets)
import System.Environment

data IMLType = Int
            deriving Show

data IMLFlowMode = In | Out | InOut
            deriving Show

data IMLChangeMode = Const | Mutable
            deriving Show

data IMLVal = Program IMLVal [IMLVal] [IMLVal] [IMLVal] -- Name [Parameters] [Functions] [Statements]
            | Ident String
            | IdentDeclaration IMLChangeMode IMLVal IMLType
            | ParamDeclaration IMLFlowMode IMLChangeMode IMLVal IMLType
            | Message String
            | FunctionDeclaration IMLVal [IMLVal] [IMLVal] -- Name [Parameters] [Statements]
            | FunctionCall IMLVal [IMLVal] -- Name [Parameters]
            | If IMLVal [IMLVal] [IMLVal]
            deriving Show

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
    <|> try parseIdentDeclaration
    <|> try parseFunctionCall
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
    -- HERE BUILD IN LITERALS
    params <- brackets (parseIdent `sepBy` (string ","))
    spaces
    char ';'
    return $ FunctionCall identName params

parseIf :: Parser IMLVal
parseIf = do
    spaces
    string "if"
    condition <- parseBraketStatement
    ifStatements <- braces parseStatementList
    elseStatements <- parseElse
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
    return $ Message "TODO"

parseFor :: Parser IMLVal
parseFor = do
    spaces
    string "for"
    return $ Message "TODO"


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
parseFlowMode = option InOut $ do 
    (string "in")
    return In
    <|> do 
    (string "out")
    return Out
    <|> do 
    (string "inout")
    return InOut

parseChangeMode :: Parser IMLChangeMode
parseChangeMode = try parseVal
    <|> parseVar
    <|> option Mutable parseConst

parseVal :: Parser IMLChangeMode
parseVal = do 
    string "val"
    return Const

parseVar :: Parser IMLChangeMode
parseVar = do 
    string "var"
    return Mutable

parseConst :: Parser IMLChangeMode
parseConst = do 
    string "const"
    return Const

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
parseType = do
    string "int"
    return Int

parseIdent :: Parser IMLVal
parseIdent = do
                head <- oneOf identStartChars
                tail <- many $ oneOf identChars
                return $ Ident (head : tail)

main :: IO()
main = print . readExpr $ "program HambbeKoenig {}"


