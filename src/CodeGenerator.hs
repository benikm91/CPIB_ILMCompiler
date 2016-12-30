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

-- TODO WHY DO WE HAVE TO HAVE SUCH SHIT!!!
fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not left"


loadIm32 :: Integer -> Instruction
loadIm32 val = LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 val))

input32 :: String -> Instruction
input32 name = Input (IntTy 32) mempty name

output32 :: String -> Instruction
output32 name = Output (IntTy 32) name

-- data Scope = Local | Global
-- data Access = Direct | Indirect
type Address = Int

data IdentInfo = Param IMLFlowMode IMLChangeMode
               | Var IMLChangeMode
               | Function

type Ident = (String, Address, IdentInfo)

type Scope = (Address, [Ident])

-- stack of scopes
type Enviroment = (Scope, [Scope]) -- Global and Locals

addLocalIdent :: Enviroment -> Ident -> Enviroment
-- TODO Check if Ident already exists => Throw error
addLocalIdent = error "HALLO2" 

getIdent :: Enviroment -> String -> Ident
getIdent = error "HALLO"

toArray :: [a] -> Array Int a
toArray l = array (0, length l - 1)  (zip [0 .. length l - 1] l)

toHaskellVM :: IMLVal -> VMProgram
toHaskellVM (Program (Ident name) params functions statements) = (name, toArray codeArray)
    where codeArray = functionInstructions ++ inputInstructions ++ statementInstructions ++ outputInstructions ++ [Stop]
          (inputInstructions, inputScope) = generateInputs params
          (functionInstructions, functionsScope) = generateFunctions functions
          (statementInstructions, programEndScope) = generateScopeCode statements (globalScope, [localScope])
          outputInstructions = generateOutputs (fst programEndScope, (reverse . snd) programEndScope)
          globalScope = functionsScope
          localScope = inputScope
toHaskellVM _ = error "Input is not a Program"

generateOutputs :: Scope -> [Instruction]
generateOutputs (sp, []) = []
generateOutputs (sp, (name, _, Param Out _) : scope) = output32 name : generateOutputs (sp - 1, scope)
generateOutputs (sp, (name, _, Param InOut _) : scope) = output32 name : generateOutputs (sp - 1, scope)
generateOutputs (sp, _ : scope) = generateOutputs (sp - 1, scope)

generateInputs :: [IMLVal] -> ([Instruction], Scope)
generateInputs = foldl connectInputs ([], (0, []))

-- TODO better name
connectInputs :: ([Instruction], Scope) -> IMLVal -> ([Instruction], Scope)
connectInputs (instructions, (sp, scope)) val = (instructions ++ newInstructions, (sp + 1, newIdent : scope))
    where (newInstructions, newIdent) = generateInput val sp

generateInput :: IMLVal -> Address -> ([Instruction], Ident)
generateInput p@(ParamDeclaration flowMode changeMode (Ident name) _) sp = (inputCode, (name, sp, Param flowMode changeMode))
    where inputCode = generateInputCode p

generateInputCode :: IMLVal -> [Instruction]
generateInputCode (ParamDeclaration Out  _ (Ident name) _) = [ loadIm32 0 ]
generateInputCode (ParamDeclaration _    _ (Ident name) _) = [ input32 name ]

generateFunctions :: [IMLVal] -> ([Instruction], Scope) -- uses generateCode
generateFunctions [] = ([], (0, [])) 
generateFunctions _ = error "TODO"

-- HERE THE LOCAL ENVIROMENT GETS UPDATED
generateScopeCode :: [IMLVal] -> Enviroment -> ([Instruction], Scope)
generateScopeCode [] (_, localScope : _) = ([], localScope) 
generateScopeCode _ _ = error "TODO"

-- generateScopeCode p@(IdentDecleration ___ : rest) enviroment = instructions ++ (generateScopeCode rest (addLocalIdent enviroment "lala"))
--    where instructions = generateCode p

generateCode :: IMLVal -> Enviroment -> [Instruction]
generateCode = error "not implemented"

program :: (Array Int Instruction)
-- program = array (0, 4) [(0, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 4))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]
program = array (0, 4) [(0, Input (IntTy 32) (rc2loc (1,1)) "Test"), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]