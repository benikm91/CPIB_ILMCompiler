module Checker
    ( check, Type(..)
    ) where

import Parser
import Text.ParserCombinators.Parsec hiding (spaces)

type Message = String

data Type = BoolType 
          | IntType 
          | IntClampType 
          | IntArrayType 
          | FunctionType [Type]
          | None
    deriving(Eq)

instance Show Type where
  show BoolType       = "bool"
  show IntType        = "int"
  show IntClampType   = "intClamp"
  show IntArrayType   = "intArray"
  show (FunctionType _) = "function"
  show None           = "none"

type Symbol = (String, Type)

type SymbolTable = [Symbol]

type Environment = ([SymbolTable])

getIdentType :: String -> Environment -> SourcePos -> Type
getIdentType ident [] pos = error $ "Ident " ++ ident ++ " not found in current scope!" ++ show pos
getIdentType ident (symbolTable : symbolTables) pos =  case maybeT of 
                                                        Just t -> t
                                                        Nothing -> getIdentType ident symbolTables pos
    where maybeT =  getIdentType2 ident symbolTable pos

getIdentType2 :: String -> SymbolTable -> SourcePos -> Maybe Type
getIdentType2 ident [] pos = Nothing --error $ "Ident " ++ ident ++ " not found in current scope!" ++ show pos
getIdentType2 ident ((currIdent, currType) : symbolTable) pos
    | currIdent == ident = Just currType
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


addFunctionIdent :: String -> [IMLVal] -> Environment -> SourcePos -> Environment
addFunctionIdent ident params environment pos = addIdent ident (FunctionType paramTypes) environment pos
    where paramTypes = map getParamType params

getParamType :: IMLVal -> Type
getParamType (ParamDeclaration _ _ _ Int _) = IntType
getParamType (ParamDeclaration _ _ _ (ClampInt _ _) _) = IntClampType
getParamType (ParamDeclaration _ _ _ (ArrayInt _ _) _) = IntArrayType

createScope :: Environment -> Environment
createScope environment = [] : environment

removeScope :: Environment -> Environment
removeScope (_ : environment) = environment

check :: IMLVal -> (Type, Environment)
check program = checkType program []

checkTypeMultiple :: [IMLVal] -> Environment -> (Type, Environment)
checkTypeMultiple [] symbolTable = (None, symbolTable)
checkTypeMultiple (v : vals) symbolTable = checkTypeMultiple vals symbol1
    where (_, symbol1) = checkType v symbolTable

checkType :: IMLVal -> Environment -> (Type, Environment)
checkType (Program _ params functions statements pos) symbolTable = (t, symbol3)
    where (_, symbol1) = checkTypeMultiple params symbolTable
          (_, symbol2) = checkTypeMultiple functions symbol1
          (t, symbol3) = checkTypeMultiple statements symbol2
checkType (Ident name pos) symbolTable = ((getIdentType name symbolTable pos), symbolTable)
checkType (IdentDeclaration _ (Ident name _) (ArrayInt a b) pos) symbolTable
    | b <= a = error ("Max index of " ++ show IntArrayType ++ " must be greater than min index! " ++ show pos)
    | otherwise = (None, (addIdent name IntArrayType symbolTable pos))
checkType (IdentDeclaration _ (Ident name _) (ClampInt a b) pos) symbolTable
    | b <= a = error ("Max of " ++ show IntClampType ++ " must be greater than min! " ++ show pos)
    | otherwise = (None, (addIdent name IntClampType symbolTable pos))
checkType (IdentDeclaration _ (Ident name _) _ pos) symbolTable = (None, (addIdent name IntType symbolTable pos))
checkType (ParamDeclaration _ _ (Ident name _) (ArrayInt a b) pos) symbolTable 
    | b <= a = error ("Max index of " ++ show IntArrayType ++ " must be greater than min index! " ++ show pos)
    | otherwise = (None, (addIdent name IntArrayType symbolTable pos))
checkType (ParamDeclaration _ _ (Ident name _) (ClampInt a b) pos) symbolTable 
    | b <= a = error ("Max index of " ++ show IntClampType ++ " must be greater than min! " ++ show pos)
    | otherwise = (None, (addIdent name IntClampType symbolTable pos))
checkType (ParamDeclaration _ _ (Ident name _) _ pos) symbolTable = (None, (addIdent name IntType symbolTable pos))
checkType (IdentFactor expression _ pos) symbolTable = checkType expression symbolTable
checkType (IdentArray (Ident name _) indexExpr pos) symbolTable
    | getIdentType name symbolTable pos /= IntArrayType = error $ "Invalid array identifier " ++ name ++ ", actual type " ++ show (getIdentType name symbolTable pos) ++ "! " ++ show pos
    | indexType /= IntType && indexType /= IntClampType = error $ "Illegal index type: " ++ show IntType ++ " expected, " ++ show indexType ++ " found! " ++ show pos
    | otherwise = (IntType, symbolTable2)
    where (indexType, symbolTable2) = checkType indexExpr symbolTable
checkType (Literal literal pos) symbolTable = (getLiteralType(literal), symbolTable)
checkType (MonadicOpr op a pos) symbolTable = checkTypeMonadic op a pos symbolTable
checkType (DyadicOpr op a b pos) symbolTable = checkTypeDyadic op a b pos symbolTable
checkType (ExprList [] pos) symbolTable = (None, symbolTable)
checkType (ExprList (expr : exprs) pos) symbolTable = checkType (ExprList exprs pos) symbolTable2
    where (_, symbolTable2) = checkType expr symbolTable
checkType (FunctionDeclaration  (Ident ident _) params statements pos) symbolTable = (t, symbol3)
    where (_, symbol1) = checkTypeMultiple params []
          (t, symbol2) = checkTypeMultiple statements symbol1
          symbol3 = addFunctionIdent ident params symbolTable pos
checkType (FunctionCall (Ident ident _) params pos) symbolTable  -- TODO: check if correct funtion arguments!
    | expectedParamTypes /= parmTypes = error $ "Illegal function call : parameter types [" ++ showTypes expectedParamTypes ", " ++ "] expected, [" ++ showTypes parmTypes ", " ++ "] found! " ++ show pos
    | otherwise = (None, symbolTable)
    where parmTypes = map (\p -> case checkType p symbolTable of (t, _) -> t)  params
          expectedParamTypes = case getIdentType ident symbolTable pos of
                                    (FunctionType p) -> p
                                    otherwise -> error $  ident ++ " is nit a function! " ++ show pos
checkType (If condition ifStatments elseStatements pos) symbolTable 
    | condType /= BoolType = error $ "Illegal if condition type : " ++ show BoolType ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (t, removeScope symbol3)
    where (condType, symbol1) = checkType condition (createScope symbolTable)
          (_, symbol2) = checkTypeMultiple ifStatments symbol1
          (t, symbol3) = checkTypeMultiple elseStatements symbol2
checkType (While condition statements pos) symbolTable 
    | condType /= BoolType = error $ "Illegal While condition type : " ++ show BoolType ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (t, removeScope symbol2)
    where (condType, symbol1) = checkType condition (createScope symbolTable)
          (t, symbol2) = checkTypeMultiple statements symbol1
checkType (For condition statements pos) symbolTable 
    | condType /= IntClampType = error $ "Illegal For condition type : " ++ show IntClampType ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (t, removeScope symbol2)
    where (condType, symbol1) = checkType condition (createScope symbolTable)
          (t, symbol2) = checkTypeMultiple statements symbol1
checkType (Assignment (Ident ident _) expression pos) symbolTable 
    | condType /= identType2 = error $ "Illegal assignment type type : " ++ show identType2 ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (None, symbolTable)
    where (condType, _) = checkType expression symbolTable
          identType = getIdentType ident symbolTable pos
          identType2 = if identType == IntArrayType || identType == IntClampType then IntType else identType
checkType _ symbolTable = (None, symbolTable)

checkTypeMonadic :: IMLOperation -> IMLVal -> SourcePos -> Environment -> (Type, Environment)
checkTypeMonadic op a pos symbolTable
    | not (any (\t -> t == typeA) expectedTypesIn) = error $ showTypes expectedTypesIn " or " ++ " expected, " ++ show typeA ++ " found! " ++ show pos
    | otherwise = (typeOut, symbol1)
    where (expectedTypesIn, typeOut) = getOpExpectedType op
          (typeA, symbol1) = checkType a symbolTable

checkTypeDyadic :: IMLOperation -> IMLVal -> IMLVal -> SourcePos -> Environment -> (Type, Environment)
checkTypeDyadic Parser.Div a (Literal (IMLInt 0) _) pos symbolTable = error $ "division by zero! " ++ show pos
checkTypeDyadic op a b pos symbolTable
    | not (any (\t -> t == typeA) expectedTypesIn) = error $ showTypes expectedTypesIn " or " ++ " expected, " ++ show typeA ++ " found! " ++ show pos
    | not (any (\t -> t == typeB) expectedTypesIn) = error $ showTypes expectedTypesIn " or " ++ " expected, " ++ show typeB ++ " found! " ++ show pos
    | otherwise = (typeOut, symbol2)
    where (expectedTypesIn, typeOut) = getOpExpectedType op
          (typeA, symbol1) = checkType a symbolTable
          (typeB, symbol2) = checkType b symbol1

showTypes :: [Type] -> String -> String
showTypes [] _ = ""
showTypes (tcType : []) _ = show tcType
showTypes (tcType : tcTypes) sep = show tcType ++ sep ++ showTypes tcTypes sep

getOpExpectedType :: IMLOperation -> ([Type] ,Type)
getOpExpectedType Parser.Plus  = ([IntType, IntClampType], IntType)
getOpExpectedType Parser.Minus = ([IntType, IntClampType], IntType)
getOpExpectedType Parser.Times = ([IntType, IntClampType], IntType)
getOpExpectedType Parser.Div   = ([IntType, IntClampType], IntType)
getOpExpectedType Parser.Mod   = ([IntType, IntClampType], IntType)
getOpExpectedType Parser.Lt    = ([IntType, IntClampType], BoolType)
getOpExpectedType Parser.Ge    = ([IntType, IntClampType], BoolType)
getOpExpectedType Parser.Eq    = ([IntType, IntClampType], BoolType)
getOpExpectedType Parser.Ne    = ([IntType, IntClampType], BoolType)
getOpExpectedType Parser.Gt    = ([IntType, IntClampType], BoolType)
getOpExpectedType Parser.Le    = ([IntType, IntClampType], BoolType)
getOpExpectedType Parser.And   = ([BoolType], BoolType)
getOpExpectedType Parser.Or    = ([BoolType], BoolType)
getOpExpectedType Parser.Not   = ([BoolType], BoolType)

getLiteralType :: IMLLiteral -> Type
getLiteralType (Parser.IMLBool _) = BoolType
getLiteralType (Parser.IMLInt _)  = IntType