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
data Access = Direct | Indirect
type Address = Int

type Ident = (String, (Address, Access))

type Scope = [Ident]

-- stack of scopes
type Enviroment = (Scope, [Scope]) -- Global and Locals

addLocalIdent :: Enviroment -> Ident -> Enviroment
-- TODO Check if Ident already exists => Throw error
addLocalIdent = error "HALLO2" 

getIdent :: Enviroment -> String -> Ident
getIdent = error "HALLO"

toHaskellVM :: IMLVal -> VMProgram
toHaskellVM (Program name params functions statements) = error "TODO" -- uses for statements generateCode

-- generateInputs :: [IMLVal] -> ([Instruction], Scope)
-- generateFunctions :: [IMLVal] -> ([Instruction], Scope) -- uses generateCode 

generateCode :: IMLVal -> Enviroment -> [Instruction]
generateCode = error "not implemented"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not left"

program :: (Array Int Instruction)
-- program = array (0, 4) [(0, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 4))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]
program = array (0, 4) [(0, Input (IntTy 32) (rc2loc (1,1)) "Test"), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]