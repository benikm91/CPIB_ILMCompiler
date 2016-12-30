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

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- TODO WHY DO WE HAVE TO HAVE SUCH SHIT!!!
fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not left"

neg :: Instruction
neg = Neg Int32VmTy mempty

loadAddress :: Int -> Instruction
loadAddress addr = LoadIm IntVmTy (IntVmVal addr)

loadIm32 :: Integer -> Instruction
loadIm32 val = LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 val))

input32 :: String -> Instruction
input32 name = Input (IntTy 32) mempty name

output32 :: String -> Instruction
output32 name = Output (IntTy 32) name

deref :: Instruction
deref = Deref

store :: Instruction
store = Store

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

findInScope :: Scope -> String -> Maybe Ident
findInScope (_, []) _ = Nothing
findInScope (sp, next : rest) name = if (fst3 next) == name then Just next else findInScope (sp, rest) name 

getIdent :: Enviroment -> String -> Ident
getIdent (global, []) name = case findInScope global name of 
    Nothing -> error $ "Identifier " ++ name ++ " not found!"
    Just a -> a
getIdent (global, next : rest) name = case findInScope next name of
    Nothing -> getIdent (global, rest) name
    Just a -> a

getIdentAddress :: Enviroment -> String -> Address
getIdentAddress a b = (snd3 (getIdent a b))

toArray :: [a] -> Array Int a
toArray l = array (0, length l - 1)  (zip [0 .. length l - 1] l)

toHaskellVM :: IMLVal -> VMProgram
toHaskellVM (Program (Ident name) params functions statements) = (name, toArray codeArray)
    where codeArray = functionInstructions ++ inputInstructions ++ statementInstructions ++ outputInstructions ++ [Stop]
          (inputInstructions, inputScope) = generateInputs params
          (functionInstructions, functionsScope) = generateFunctions functions
          statementInstructions = generateScopeCode statements (globalScope, [localScope])
          outputInstructions = generateOutputs inputScope
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
generateScopeCode :: [IMLVal] -> Enviroment -> [Instruction]
generateScopeCode [] _ = [] 
generateScopeCode (next : rest) enviroment = newInstructions ++ generateScopeCode rest newEnviroment
    where (newInstructions, newEnviroment) = generateCode next enviroment 

-- generateScopeCode p@(IdentDecleration ___ : rest) enviroment = instructions ++ (generateScopeCode rest (addLocalIdent enviroment "lala"))
--    where instructions = generateCode p

generateCode :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateCode (Ident name) env = ([ loadAddress $ getIdentAddress env name, deref ], env)
generateCode (MonadicOpr Parser.Minus expression) env = (expressionInstructions ++ [neg], newEnviroment)
    where (expressionInstructions, newEnviroment) = generateCode expression env
generateCode (Assignment (Ident name) expression) env = ([loadAddress  $ getIdentAddress env name] ++ expressionInstructions ++ [store], newEnviroment)
    where (expressionInstructions, newEnviroment) = generateCode expression env
generateCode (IdentFactor ident Nothing) env = generateCode ident env
generateCode s _ = error $ "not implemented" ++ show s

program :: (Array Int Instruction)
-- program = array (0, 4) [(0, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 4))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]
program = array (0, 4) [(0, Input (IntTy 32) (rc2loc (1,1)) "Test"), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]