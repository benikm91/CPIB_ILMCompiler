module CodeGenerator
    ( toHaskellVM
    ) where

import Parser
import BaseDecls
import VirtualMachineIO
import Locations
import Data.Array
import Data.Monoid
import CheckedArithmetic

-- data Scope = Local | Global
-- data Access = Direct | Indirect
type Address = Int

type Ident = (String, (Address, IMLChangeMode))

type Scope = [Ident]

-- stack of scopes
type Enviroment = (Scope, [Scope]) -- Global and Locals

addLocalIdent :: Enviroment -> Ident -> Enviroment
-- TODO Check if Ident already exists => Throw error
addLocalIdent = error "HALLO2" 

createIdent :: String -> Address -> IMLChangeMode -> Ident
createIdent name addr changeMode = (name, (addr, changeMode))

getIdent :: Enviroment -> String -> Ident
getIdent = error "HALLO"

toHaskellVM :: IMLVal -> VMProgram
toHaskellVM (Program name params functions statements) = (name, codeArray)
    where codeArray = functionInstructions ++ inputInstructions ++ statementInstructions
          (inputInstructions, inputScope) = generateInputs params
          (functionInstructions, functionsScope) = generateFunctions functions
          statementInstructions = generateScopeCode (globalScope, programScope) statements
          globalScope = functionsScope
          localScope = inputScope

generateInputs :: [IMLVal] -> ([Instruction], Scope)
generateInputs = foldl connectInputs ([], [])

-- TODO better name
connectInputs :: ([Instruction], Scope) -> IMLVal -> ([Instruction], Scope)
connectInputs (instructions, scope) val = (instructions ++ newInstructions, newIdent : scope)
    where (newInstructions, newIdent) = generateInput val

generateInput :: IMLVal -> ([Instruction], Ident)
generateInput p = (inputCode, createIdent name changeMode)
    where (inputCode, address) = generateInputCode p

generateInputCode :: IMLVal -> ([Instruction], Address)
generateInputCode (ParamDeclaration flowMode changeMode (Ident name) type) = ([ Input IntTy mempty (name) ])

generateFunctions :: [IMLVal] -> ([Instruction], Scope) -- uses generateCode
generateFunctions [] = ([], []) 
generateFunctions _ = error "TODO"

-- HERE THE LOCAL ENVIROMENT GETS UPDATED
generateScopeCode :: [IMLVal] -> Enviroment -> [Instruction]
-- generateScopeCode p@(IdentDecleration ___ : rest) enviroment = instructions ++ (generateScopeCode rest (addLocalIdent enviroment "lala"))
--    where instructions = generateCode p

generateCode :: IMLVal -> Enviroment -> [Instruction]
generateCode = error "not implemented"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not left"

program :: (Array Int Instruction)
-- program = array (0, 4) [(0, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 4))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]
program = array (0, 4) [(0, Input (IntTy 32) (rc2loc (1,1)) "Test"), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]