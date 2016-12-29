module Parser ( readExpr, IMLVal, IMLType, IMLFlowMode, IMLChangeMode ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Token
import System.Environment

data IMLType = Int
            deriving Show

data IMLFlowMode = In | Out | InOut
            deriving Show

data IMLChangeMode = Const | Mutable
            deriving Show

data IMLVal = Program [IMLVal] [IMLVal] IMLVal
            | Ident String
            | IdentDeclaration IMLFlowMode IMLChangeMode IMLVal IMLType
            | Message String
            | FunctionDeclaration IMLVal [IMLVal] --IMLVal
            deriving Show

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
    char '{'
    spaces 
    functions <- parseFunctionList
    spaces
    x <- many (noneOf "}")
    spaces
    char '}'
    return $ Program params functions name

parseFunctionList :: Parser [IMLVal]
parseFunctionList = do
    functions <- many parseFunction
    return functions

parseFunction :: Parser IMLVal
parseFunction = do
    spaces
    string "def"
    spaces
    identName <- parseIdent
    spaces
    params <- parseParamList
    spaces
    char '{'
    spaces 
    x <- many (noneOf "}")
    spaces
    char '}'
    spaces
    return $ FunctionDeclaration identName params

parseParamList :: Parser [IMLVal]
parseParamList = do
    char '('
    spaces
    params <- parseParam `sepBy` (string ",")
    spaces
    char ')'
    return params

parseParam :: Parser IMLVal
parseParam = do
    spaces
    flowMode <- parseFlowMode
    spaces
    changeMode <- parseChangeMode
    spaces
    (identName, identType) <- parseTypedIdent
    return $ IdentDeclaration flowMode changeMode identName identType

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
parseChangeMode = option Mutable parseConst

parseConst :: Parser IMLChangeMode
parseConst = do 
    string "const"
    return Const

parseTypedIdent :: Parser (IMLVal, IMLType)
parseTypedIdent = do
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