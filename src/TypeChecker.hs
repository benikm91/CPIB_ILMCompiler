module TypeChecker
    ( checkTypes, Type(..)
    ) where

import Parser
import Text.ParserCombinators.Parsec hiding (spaces)

type Message = String

data Type = TCBool | TCInt | None
    deriving(Eq)

instance Show Type where
  show TCBool = "bool"
  show TCInt  = "int"
  show None   = "none"

type Symbol = (String, Type)

getIdentType :: String -> [Symbol] -> SourcePos -> Type
getIdentType ident [] pos = error $ "Ident " ++ ident ++ " not found!" ++ show pos
getIdentType ident ((currIdent, currType) : symbolTable) pos
            | currIdent == ident = currType
            | otherwise = getIdentType ident symbolTable pos

addIdent :: String -> Type -> [Symbol] -> [Symbol]
addIdent ident identType symbolTable = (ident, identType) : symbolTable

checkTypes :: IMLVal -> (Type, [Symbol])
checkTypes program = checkType program []

checkTypeMultiple :: [IMLVal] -> [Symbol] -> [Symbol]
checkTypeMultiple [] symbolTable = symbolTable
checkTypeMultiple (v : vals) symbolTable = checkTypeMultiple vals symbol1
    where (_, symbol1) = checkType v symbolTable

checkType :: IMLVal -> [Symbol] -> (Type, [Symbol])
checkType (Program _ params functions statements pos) symbolTable = (None, symbol3)
    where symbol1 = checkTypeMultiple params symbolTable
          symbol2 = checkTypeMultiple functions symbol1
          symbol3 = checkTypeMultiple statements symbol2
checkType (Ident name pos) symbolTable = ((getIdentType name symbolTable pos), symbolTable)
checkType (IdentDeclaration _ (Ident name _) _ _) symbolTable = (None, (addIdent name TCInt symbolTable))
checkType (ParamDeclaration _ _ (Ident name _) _ _) symbolTable = (None, (addIdent name TCInt symbolTable))
checkType (IdentFactor expression _ pos) symbolTable = checkType expression symbolTable --TODO: correct??
checkType (IdentArray (Ident name _) indexExpr pos) symbolTable
    | getIdentType name symbolTable pos /= TCInt = error $ "Invalid array identifier" ++ show pos
    | indexType /= TCInt = error $ "Illegal index type: " ++ show TCInt ++ " expected, " ++ show indexType ++ " found! " ++ show pos
    | otherwise = (None, symbolTable2)
    where (indexType, symbolTable2) = checkType indexExpr symbolTable
checkType (Literal literal pos) symbolTable = (getLiteralType(literal), symbolTable)
checkType (MonadicOpr op a pos) symbolTable = checkTypeMonadic op a pos symbolTable
checkType (DyadicOpr op a b pos) symbolTable = checkTypeDyadic op a b pos symbolTable
checkType (ExprList [] pos) symbolTable = (None, symbolTable)
checkType (ExprList (expr : exprs) pos) symbolTable = checkType (ExprList exprs pos) symbolTable2
    where (_, symbolTable2) = checkType expr symbolTable
checkType (FunctionDeclaration _ params statements pos) symbolTable = (None, symbol2)
    where symbol1 = checkTypeMultiple params symbolTable
          symbol2 = checkTypeMultiple statements symbol1
checkType (FunctionCall _ _ _) symbolTable = (None, symbolTable) -- TODO!!!!
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
    | condType /= TCBool = error $ "Illegal For condition type : " ++ show TCBool ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (None, symbol2)
    where (condType, symbol1) = checkType condition symbolTable
          symbol2 = checkTypeMultiple statements symbol1
checkType (Assignment (Ident ident _) expression pos) symbolTable 
    | condType /= identType = error $ "Illegal assignment type type : " ++ show identType ++ " expected, " ++ show condType ++ " found! " ++ show pos
    | otherwise = (identType, symbolTable)
    where (condType, _) = checkType expression symbolTable
          identType = getIdentType ident symbolTable pos

checkTypeMonadic :: IMLOperation -> IMLVal -> SourcePos -> [Symbol] -> (Type, [Symbol])
checkTypeMonadic op a pos symbolTable
    | typeA /= expectedTypeIn = error $ (show expectedTypeIn) ++ " expected, " ++ (show typeA) ++ " found! " ++ (show pos)
    | otherwise = (typeOut, symbol1)
    where (expectedTypeIn, typeOut) = getOpExpectedType op
          (typeA, symbol1) = checkType a symbolTable

checkTypeDyadic :: IMLOperation -> IMLVal -> IMLVal -> SourcePos -> [Symbol] -> (Type, [Symbol])
checkTypeDyadic op a b pos symbolTable
    | typeA /= expectedTypeIn = error $ show expectedTypeIn ++ " expected, " ++ show typeA ++ " found! " ++ show pos
    | typeB /= expectedTypeIn = error $ show expectedTypeIn ++ " expected, " ++ show typeB ++ " found! " ++ show pos
    | otherwise = (typeOut, symbol2)
    where (expectedTypeIn, typeOut) = getOpExpectedType op
          (typeA, symbol1) = checkType a symbolTable
          (typeB, symbol2) = checkType b symbol1

getOpExpectedType :: IMLOperation -> (Type,Type)
getOpExpectedType Parser.Plus  = (TCInt, TCInt)
getOpExpectedType Parser.Minus = (TCInt, TCInt)
getOpExpectedType Parser.Times = (TCInt, TCInt)
getOpExpectedType Parser.Div   = (TCInt, TCInt)
getOpExpectedType Parser.Mod   = (TCInt, TCInt)
getOpExpectedType Parser.Lt    = (TCInt, TCBool)
getOpExpectedType Parser.Ge    = (TCInt, TCBool)
getOpExpectedType Parser.Eq    = (TCInt, TCBool)
getOpExpectedType Parser.Ne    = (TCInt, TCBool)
getOpExpectedType Parser.Gt    = (TCInt, TCBool)
getOpExpectedType Parser.Le    = (TCInt, TCBool)
getOpExpectedType Parser.And   = (TCBool, TCBool)
getOpExpectedType Parser.Or    = (TCBool, TCBool)
getOpExpectedType Parser.Not   = (TCBool, TCBool)

getLiteralType :: IMLLiteral -> Type
getLiteralType (Parser.IMLBool _) = TCBool
getLiteralType (Parser.IMLInt _)  = TCInt