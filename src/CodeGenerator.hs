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

thd3 ::  (a, b, c) -> c
thd3 (_, _, c) = c

-- TODO WHY DO WE HAVE TO HAVE SUCH SHIT!!!
fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not left"

-- Instructions

neg :: Instruction
neg = Neg Int32VmTy mempty

add32, sub32, mult32, divFloor32, eq32, ne32, gt32, ge32, lt32, le32 :: Instruction
add32 = Add Int32VmTy mempty
sub32 = Sub Int32VmTy mempty
mult32 = Mult Int32VmTy mempty
divFloor32 = DivFloor Int32VmTy mempty
eq32 = VirtualMachineIO.Eq Int32VmTy
ne32 = VirtualMachineIO.Ne Int32VmTy
gt32 = VirtualMachineIO.Gt Int32VmTy
ge32 = VirtualMachineIO.Ge Int32VmTy
lt32 = VirtualMachineIO.Lt Int32VmTy
le32 = VirtualMachineIO.Le Int32VmTy

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

condJump :: CodeAddress -> Instruction
condJump = CondJump

uncondJumpp :: CodeAddress -> Instruction
uncondJumpp = UncondJump

-- End Instruction

-- data Scope = Local | Global
-- data Access = Direct | Indirect
type Address = Int

data IdentInfo = Param IMLFlowMode IMLChangeMode
               | Var IMLChangeMode
               | Function

type Ident = (String, Address, IdentInfo)

type Scope = (Address, [Ident])

-- stack of scopes
type Enviroment = (CodeAddress, Scope, [Scope]) -- PC, Global, Locals

addLocalIdent :: Enviroment -> Ident -> Enviroment
-- TODO Check if Ident already exists => Throw error
addLocalIdent (addr, gEnv, lEnvs) ident = (addr, gEnv, addToLocalScope lEnvs ident)

addToLocalScope :: [Scope] -> Ident -> [Scope]
addToLocalScope scopes ident = others ++ [(address+1, idents ++ [ident])]
    where others = init scopes
          myScope = last scopes
          address = fst myScope
          idents = snd myScope

addNewLocalScope :: Enviroment -> Scope -> Enviroment
addNewLocalScope (addr, gScope, lScopes) scope = (addr, gScope, lScopes ++ [scope])

removeLocalScope :: Enviroment -> Scope -> Enviroment
removeLocalScope (addr, gScope, lScopes) scope  = (addr, gScope, init lScopes)

findInScope :: Scope -> String -> Maybe Ident
findInScope (_, []) _ = Nothing
findInScope (sp, next : rest) name = if (fst3 next) == name then Just next else findInScope (sp, rest) name 

getCodeAddress :: Enviroment -> Int
getCodeAddress (address, _, _) = address

updateCodeAddress :: Enviroment -> Int -> Enviroment
updateCodeAddress (addr, gs, ls) i = (addr + i, gs, ls)

-- updateSp :: Enviroment -> Int -> Enviroment
-- updateSp (gScope, lScopes) i = (gScope, others ++ [(address+i, idents)])
--     where others = init lScopes
--           myScope = last lScopes
--           address = fst myScope
--           idents = snd myScope

getIdent :: Enviroment -> String -> Ident
getIdent (_, global, []) name = case findInScope global name of 
    Nothing -> error $ "Identifier " ++ name ++ " not found!"
    Just a -> a
getIdent (ca, global, next : rest) name = case findInScope next name of
    Nothing -> getIdent (ca, global, rest) name
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
          pc = length functionInstructions + length inputInstructions
          (statementInstructions, _) = generateScopeCode (pc, globalScope, [localScope]) [] statements
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

addTopLocalScope :: [Ident] -> Enviroment -> Enviroment 
addTopLocalScope idents (pc, global, locals@((sp, _) : rest)) = (pc, global, (sp, idents) : locals)  

-- HERE THE LOCAL ENVIROMENT GETS UPDATED
generateScopeCode :: Enviroment -> [Ident] -> [IMLVal] -> ([Instruction], Enviroment)
generateScopeCode startEnv idents instructions = dropTopLocalScope $ foldl connectCode ([], addTopLocalScope idents startEnv) instructions
    where dropTopLocalScope (instructions, (pc, global, _ : locals)) = (instructions, (pc, global, locals))

connectCode :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectCode (instructions, enviroment) val = (instructions ++ newInstructions, newEnviroment)
    where (newInstructions, newEnviroment) = generateCode val enviroment

generateCode :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateCode (Ident name) env = ([ loadAddress $ getIdentAddress env name, deref ], updateCodeAddress env 2)
generateCode (Literal (IMLInt i)) env = ([loadIm32 $ toInteger i], updateCodeAddress env 1)
generateCode (MonadicOpr Parser.Minus expression) env = (expressionInstructions ++ [neg], updateCodeAddress newEnviroment 1)
    where (expressionInstructions, newEnviroment) = generateCode expression env
generateCode (Assignment (Ident name) expression) env = ([loadAddress $ getIdentAddress env name] ++ expressionInstructions ++ [store], updateCodeAddress newEnviroment 2)
    where (expressionInstructions, newEnviroment) = generateCode expression env
generateCode (IdentFactor ident Nothing) env = generateCode ident env
generateCode (DyadicOpr op a b) env = (expressionInstructions ++ [getDyadicOpr op], updateCodeAddress newEnviroment 1)
    where (expressionInstructions, newEnviroment) = (fst (generateCode a env) ++ fst (generateCode b env), snd $ generateCode b (snd $ generateCode a env))
generateCode (If condition ifStatement elseStatement) env@(_, global, locals) = (conditionInstructions ++ [condJump (getCodeAddress ifEndEnv + 1)] ++ ifStatementInstructions ++ [uncondJumpp (getCodeAddress elseEndEnv)] ++ elseStatementInstructions, elseEndEnv)
    where (conditionInstructions, condEndEnv) = generateCode condition env
          (ifStatementInstructions, ifEndEnv) = generateScopeCode (updateCodeAddress condEndEnv 1) [] ifStatement --TODO use the hole elseStament
          (elseStatementInstructions, elseEndEnv) = generateScopeCode (updateCodeAddress ifEndEnv 1) [] elseStatement --TODO use the hole elseStament
generateCode s _ = error $ "not implemented" ++ show s

-- generateCodeWithNewScope :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
-- generateCodeWithNewScope vals env = generateStatmensCode vals (addNewLocalScope env) []

-- generateStatmensCode :: [IMLVal] -> Enviroment -> [Instruction] -> ([Instruction], Enviroment)
-- generateStatmensCode [] env intructions = (intructions, removeLocalScope env)
-- generateStatmensCode (val:rest) env intructions = generateStatmensCode rest newEnv (intructions ++ newInstructions)
--     where (newInstructions, newEnv) = generateCode val env

getDyadicOpr :: IMLOperation -> Instruction
getDyadicOpr Parser.Plus = add32
getDyadicOpr Parser.Minus = sub32
getDyadicOpr Parser.Times = mult32
getDyadicOpr Parser.Div = divFloor32
getDyadicOpr Parser.Lt = lt32
getDyadicOpr Parser.Ge = ge32
getDyadicOpr Parser.Eq = eq32
getDyadicOpr Parser.Ne = ne32
getDyadicOpr Parser.Gt = gt32
getDyadicOpr Parser.Le = le32
getDyadicOpr Parser.And = error "TODO"
getDyadicOpr Parser.Or = error "TODO"
getDyadicOpr Parser.Not = error "TODO"

program :: (Array Int Instruction)
-- program = array (0, 4) [(0, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 4))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]
program = array (0, 4) [(0, Input (IntTy 32) (rc2loc (1,1)) "Test"), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]