module TypeChecker
    ( checkTypes, Type(..)
    ) where

import Parser
import Text.ParserCombinators.Parsec hiding (spaces)

type Message = String

data Type = TCBool | TCInt | TCIntClamp | TCIntArray | None
    deriving(Eq)

instance Show Type where
  show TCBool       = "bool"
  show TCInt        = "int"
  show TCIntClamp   = "intClamp"
  show TCIntArray   = "intArray"
  show None         = "none"

type Symbol = (String, Type)

type SymbolTable = [Symbol]

type Environment = [SymbolTable]

getIdentType :: String -> Environment -> SourcePos -> Type
getIdentType ident [] pos = error $ "Ident " ++ ident ++ " not found in current scope!" ++ show pos
getIdentType ident (symbolTable : _) pos = getIdentType2 ident symbolTable pos

getIdentType2 :: String -> SymbolTable -> SourcePos -> Type
getIdentType2 ident [] pos = error $ "Ident " ++ ident ++ " not found in current scope!" ++ show pos
getIdentType2 ident ((currIdent, currType) : symbolTable) pos
    | currIdent == ident = currType
    | otherwise = getIdentType2 ident symbolTable pos

addIdent :: String -> Type -> Environment -> SourcePos -> Environment
addIdent ident identType [] pos = [addIdent2 ident identType [] pos]
addIdent ident identType (symbolTable : symbolTables) pos = (addIdent2 ident identType symbolTable pos) : symbolTables

addIdent2 :: String -> Type -> SymbolTable -> SourcePos -> SymbolTable
addIdent2 ident identType symbolTable pos
    | (isIdentDefined ident symbolTable) == True = error $ "Identifier " ++ ident ++ " already defined in current scope! " ++ show pos
    | otherwise = (ident, identType) : symbolTable

isIdentDefined :: String -> SymbolTable -> Bool
isIdentDefined ident [] = False
isIdentDefined ident ((currIdent, currType) : symbolTable)
    | currIdent == ident = True
    | otherwise = isIdentDefined ident symbolTable

createScope :: Environment -> Environment
createScope environment = [] : environment

removeScope :: Environment -> Environment
removeScope (_ : environment) = environment

checkTypes :: IMLVal -> (Type, Environment)
checkTypes program = checkType program []

checkTypeMultiple :: [IMLVal] -> Environment -> Environment
checkTypeMultiple [] symbolTable = symbolTable
checkTypeMultiple (v : vals) symbolTable = checkTypeMultiple vals symbol1
    where (_, symbol1) = checkType v symbolTable

checkType :: IMLVal -> Environment -> (Type, Environment)
checkType (Program _ params functions statements pos) symbolTable = (None, symbol3)
    where symbol1 = checkTypeMultiple params symbolTable
          symbol2 = checkTypeMultiple functions symbol1
          symbol3 = checkTypeMultiple statements symbol2
checkType (Ident name pos) symbolTable = ((getIdentType name symbolTable pos), symbolTable)
checkType (IdentDeclaration _ (Ident name _) (ArrayInt _ _) pos) symbolTable = (None, (addIdent name TCIntArray symbolTable pos))
checkType (IdentDeclaration _ (Ident name _) (ClampInt _ _) pos) symbolTable = (None, (addIdent name TCIntClamp symbolTable pos))
checkType (IdentDeclaration _ (Ident name _) _ pos) symbolTable = (None, (addIdent name TCInt symbolTable pos))
checkType (ParamDeclaration _ _ (Ident name _) (ArrayInt _ _) pos) symbolTable = (None, (addIdent name TCIntArray symbolTable pos))
checkType (ParamDeclaration _ _ (Ident name _) (ClampInt _ _) pos) symbolTable = (None, (addIdent name TCIntClamp symbolTable pos))
checkType (ParamDeclaration _ _ (Ident name _) _ pos) symbolTable = (None, (addIdent name TCInt symbolTable pos))
checkType (IdentFactor expression _ pos) symbolTable = checkType expression symbolTable --TODO: correct??
checkType (IdentArray (Ident name _) indexExpr pos) symbolTable
    | getIdentType name symbolTable pos /= TCIntArray = error $ "Invalid array identifier " ++ name ++ ", actual type " ++ show (getIdentType name symbolTable pos) ++ " " ++ show pos
    | indexType /= TCInt && indexType /= TCIntClamp = error $ "Illegal index type: " ++ show TCInt ++ " expected, " ++ show indexType ++ " found! " ++ show pos
    | otherwise = (TCInt, symbolTable2)
    where (indexType, symbolTable2) = checkType indexExpr symbolTable
checkType (Literal literal pos) symbolTable = (getLiteralType(literal), symbolTable)
checkType (MonadicOpr op a pos) symbolTable = checkTypeMonadic op a pos symbolTable
checkType (DyadicOpr op a b pos) symbolTable = checkTypeDyadic op a b pos symbolTable
checkType (ExprList [] pos) symbolTable = (None, symbolTable)
checkType (ExprList (expr : exprs) pos) symbolTable = checkType (ExprList exprs pos) symbolTable2
    where (_, symbolTable2) = checkType expr symbolTable
checkType (FunctionDeclaration _ params statements pos) symbolTable = (None, (removeScope symbol2))
    where symbol1 = checkTypeMultiple params (createScope symbolTable)
          symbol2 = checkTypeMultiple statements symbol1
checkType (FunctionCall _ _ _) symbolTable = (None, symbolTable) -- TODO: check if correct funtion arguments!
checkType (If condition ifStatments elseStatements pos) symbolTable 
    | condType /= TCBool = error $ "Illegal if condition type : " ++ show TCBool ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (None, symbol3)
    where (condType, symbol1) = checkType condition symbolTable
          symbol2 = checkTypeMultiple ifStatments symbol1
          symbol3 = checkTypeMultiple elseStatements symbol2
checkType (While condition statements pos) symbolTable 
    | condType /= TCBool = error $ "Illegal While condition type : " ++ show TCBool ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (None, symbol2)
    where (condType, symbol1) = checkType condition symbolTable
          symbol2 = checkTypeMultiple statements symbol1
checkType (For condition statements pos) symbolTable 
    | condType /= TCIntClamp = error $ "Illegal For condition type : " ++ show TCIntClamp ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (None, symbol2)
    where (condType, symbol1) = checkType condition symbolTable
          symbol2 = checkTypeMultiple statements symbol1
checkType (Assignment (Ident ident _) expression pos) symbolTable 
    | condType /= identType2 = error $ "Illegal assignment type type : " ++ show identType2 ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (None, symbolTable)
    where (condType, _) = checkType expression symbolTable
          identType = getIdentType ident symbolTable pos
          identType2 = if identType == TCIntArray || identType == TCIntClamp then TCInt else identType
checkType _ symbolTable = (None, symbolTable)

checkTypeMonadic :: IMLOperation -> IMLVal -> SourcePos -> Environment -> (Type, Environment)
checkTypeMonadic op a pos symbolTable
    | not (any (\t -> t == typeA) expectedTypesIn) = error $ showTypes expectedTypesIn ++ " expected, " ++ show typeA ++ " found! " ++ show pos
    | otherwise = (typeOut, symbol1)
    where (expectedTypesIn, typeOut) = getOpExpectedType op
          (typeA, symbol1) = checkType a symbolTable

checkTypeDyadic :: IMLOperation -> IMLVal -> IMLVal -> SourcePos -> Environment -> (Type, Environment)
checkTypeDyadic op a b pos symbolTable
    | not (any (\t -> t == typeA) expectedTypesIn) = error $ showTypes expectedTypesIn ++ " expected, " ++ show typeA ++ " found! " ++ show pos
    | not (any (\t -> t == typeB) expectedTypesIn) = error $ showTypes expectedTypesIn ++ " expected, " ++ show typeB ++ " found! " ++ show pos
    | otherwise = (typeOut, symbol2)
    where (expectedTypesIn, typeOut) = getOpExpectedType op
          (typeA, symbol1) = checkType a symbolTable
          (typeB, symbol2) = checkType b symbol1

showTypes :: [Type] -> String
showTypes [] = ""
showTypes (tcType : []) = show tcType
showTypes (tcType : tcTypes) = show tcType ++ " or " ++ showTypes tcTypes

getOpExpectedType :: IMLOperation -> ([Type] ,Type)
getOpExpectedType Parser.Plus  = ([TCInt, TCIntClamp], TCInt)
getOpExpectedType Parser.Minus = ([TCInt, TCIntClamp], TCInt)
getOpExpectedType Parser.Times = ([TCInt, TCIntClamp], TCInt)
getOpExpectedType Parser.Div   = ([TCInt, TCIntClamp], TCInt)
getOpExpectedType Parser.Mod   = ([TCInt, TCIntClamp], TCInt)
getOpExpectedType Parser.Lt    = ([TCInt, TCIntClamp], TCBool)
getOpExpectedType Parser.Ge    = ([TCInt, TCIntClamp], TCBool)
getOpExpectedType Parser.Eq    = ([TCInt, TCIntClamp], TCBool)
getOpExpectedType Parser.Ne    = ([TCInt, TCIntClamp], TCBool)
getOpExpectedType Parser.Gt    = ([TCInt, TCIntClamp], TCBool)
getOpExpectedType Parser.Le    = ([TCInt, TCIntClamp], TCBool)
getOpExpectedType Parser.And   = ([TCBool], TCBool)
getOpExpectedType Parser.Or    = ([TCBool], TCBool)
getOpExpectedType Parser.Not   = ([TCBool], TCBool)

getLiteralType :: IMLLiteral -> Type
getLiteralType (Parser.IMLBool _) = TCBool
getLiteralType (Parser.IMLInt _)  = TCInt