import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


readExpr :: String -> IMLVal
readExpr input = case parse parseProgram "Hambbe" input of
    Left err -> Message $ "fuck you: " ++ show err
    Right val -> val

data IMLVal = Program IMLVal
            | Ident String
            | FlowMode FlowModeType
            | Message String
            deriving Show


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
    string "program"
    spaces
    name <- parseIdent 
    spaces
    char '{'
    spaces 
    x <- many (noneOf "}")
    spaces
    char '}'
    return $ Program name

progParamList :: Parse IMLVal
progParamList = do
    char '('
    spaces
    params <- progParam `sepBy` (symbol ",")
    spaces
    char ')'
    return 

progParam :: Parse IMLVal
progParam = do
    optionMaybe 

flowMode :: Parse IMLVal
flowMode = 
    

_changeModeOptional :: Parse IMLVal
_changeModeOptional = do


	(program,
		[[T PROGRAM, T IDENT, N progParamList, T LBRACE, N cpsCmd, T RBRACE],
		 [T PROGRAM, T IDENT, N progParamList, T GLOBAL, N cpsDecl, T LBRACE, N cpsCmd, T RBRACE]]),

	(progParamList, 
		[[T LPAREN, N underlineprogParamOptional, T RPAREN]]),

	(progParam, 
		[[N underlineflowModeOptional, N underlinechangeModeOptional, T typedIdent]]),
        
	(underlineprogParamOptional, 
		[[N progParam, N underlineprogParamMany], 
        
	(underlineflowModeOptional, 
		[[], [T FLOWMODE]]),

        

parseIdent :: Parser IMLVal
parseIdent = do
                head <- oneOf identStartChars
                tail <- many $ oneOf identChars
                return $ Ident (head : tail)

mustafa2 :: String
mustafa2 = "Hambbe ist der König dieser Welt!"

mustafa3 :: IMLVal
mustafa3 = readExpr "program HambbeKoenig {}"

main :: IO()
main = print . readExpr $ "program HambbeKoenig {}"