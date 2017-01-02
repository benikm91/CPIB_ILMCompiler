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

call :: CodeAddress -> Instruction
call = Call

store :: Instruction
store = Store

condJump :: CodeAddress -> Instruction
condJump = CondJump

uncondJump :: CodeAddress -> Instruction
uncondJump = UncondJump

-- End Instruction

-- data Scope = Local | Global
-- data Access = Direct | Indirect
type Address = Int

data IdentInfo = Param IMLFlowMode IMLChangeMode
               | Var IMLType IMLChangeMode
               | Function Scope Scope -- input Scope function Scope

type Ident = (String, Address, IdentInfo)

type Scope = [Ident]

-- stack of scopes
type Enviroment = (CodeAddress, Address, Scope, [Scope]) -- PC, Global, Locals

updateCodeAddress :: Enviroment -> Int -> Enviroment
updateCodeAddress (pc, sp, global, locals) i = (pc + i, sp, global, locals)

addLocalIdent :: Enviroment -> Ident -> Enviroment
-- TODO Check if Ident already exists => Throw error
addLocalIdent (pc, sp, global, locals) ident = (pc, sp + 1, global, addToLocalScope locals ident)

addLocalScope :: Scope -> Enviroment -> Enviroment 
addLocalScope scope (pc, sp, global, locals) = (pc, sp, global, scope : locals)  

addToLocalScope :: [Scope] -> Ident -> [Scope]
addToLocalScope (next : rest) ident = (ident : next) : rest 

addToGlobalScope :: Enviroment -> Ident -> Enviroment
addToGlobalScope (pc, sp, global, locals) ident = (pc, sp, ident : global, locals)

removeLocalScope :: Enviroment -> Enviroment
removeLocalScope (pc, sp, global, locals)  = (pc, sp, global, init locals)

getPc :: Enviroment -> Int
getPc (pc, _, _, _) = pc

getLocalScopes :: Enviroment -> [Scope]
getLocalScopes (_, _, _, locals) = locals 

findInScope :: Scope -> String -> Maybe Ident
findInScope [] _ = Nothing
findInScope (next : rest) name = if (fst3 next) == name then Just next else findInScope rest name 

getIdent :: Enviroment -> String -> Ident
getIdent (_, _, global, []) name = case findInScope global name of 
    Nothing -> error $ "Identifier " ++ name ++ " not found!"
    Just a -> a
getIdent (pc, sp, global, next : rest) name = case findInScope next name of
    Nothing -> getIdent (pc, sp, global, rest) name
    Just a -> a

getIdentAddress :: Enviroment -> String -> Address
getIdentAddress a b = (snd3 (getIdent a b))

toArray :: [a] -> Array Int a
toArray l = array (0, length l - 1)  (zip [0 .. length l - 1] l)

emptyEnviroment :: Enviroment
emptyEnviroment = (0, 0, [], [])

toHaskellVM :: IMLVal -> VMProgram
toHaskellVM (Program (Ident name) params functions statements) = (name, toArray codeArray)
    where codeArray = functionInstructions ++ inputInstructions ++ statementInstructions ++ outputInstructions ++ [Stop]
          (functionInstructions, functionEndEnv) = generateFunctions functions emptyEnviroment  
          (inputInstructions, inputEndEnv) = generateInputs params functionEndEnv 
          (statementInstructions, _) = generateScopeCode statements inputEndEnv 
          outputInstructions = generateOutputs ((head . getLocalScopes) inputEndEnv)
toHaskellVM _ = error "Input is not a Program"

generateOutputs :: Scope -> [Instruction]
generateOutputs [] = []
generateOutputs ((name, _, Param Out _) : rest) = output32 name : generateOutputs rest
generateOutputs ((name, _, Param InOut _) : rest) = output32 name : generateOutputs rest
generateOutputs (_ : rest) = generateOutputs rest

generateInputs :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
generateInputs statements startEnv = foldl connectInput ([], addLocalScope [] startEnv) statements

-- TODO better name
connectInput :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectInput (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateInput statement env

generateInput :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateInput p@(ParamDeclaration flowMode changeMode (Ident name) _) env@(pc, sp, global, locals) = (newInstructions, newEnv)
    where newInstructions = generateInputCode p
          newIdent = (name, sp, Param flowMode changeMode)
          newEnv = (pc + length newInstructions, sp + 1, global, addToLocalScope locals newIdent)

generateInputCode :: IMLVal -> [Instruction]
generateInputCode (ParamDeclaration Out  _ (Ident name) _) = [ loadIm32 0 ]
generateInputCode (ParamDeclaration _    _ (Ident name) _) = [ input32 name ]

generateFunctions :: [IMLVal] -> Enviroment ->  ([Instruction], Enviroment)
generateFunctions [] env = ([], env)
generateFunctions statements startEnv = (instructions, newEnv)
    where (instructions, newEnv) = foldl connectFunction ([], startEnv) statements

connectFunction :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectFunction (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, functionEnv, inputScope) = generateFunction statement env
          newIdent = Function inputScope ((head . getLocalScopes) functionEnv)
          -- add the function to the global scope and remove the last local scope (which is from the function)
          newEnv = removeLocalScope $ addToGlobalScope functionEnv newIdent 

generateFunction :: IMLVal -> Enviroment -> ([Instruction], Enviroment, Scope)
generateFunction (FunctionDeclaration name params statements) env@(pc, sp, global, locals) = (instructions, newEnv, (head . getLocalScopes) inputEndEnv)
    where (paramInstructions, inputEndEnv) = generateFunctionInputs (reverse params) sp
          (statementInstructions, functionEndEnv) = generateMultiCode inputEndEnv statements
          (returnInstruction, returnEndEnv) = (Return 0, updateCodeAddress functionEndEnv 1)
          instructions = paramInstructions ++ statementInstructions ++ returnInstruction
          newEnv = returnEndEnv
    
generateFunctionInputs :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
generateFunctionInputs statements startEnv = foldl connectFunctionInput ([], addLocalScope [] startEnv) statements

connectFunctionInput :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectFunctionInput (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateFunctionInput statement env

generateFunctionInput :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateFunctionInput p@(ParamDeclaration flowMode changeMode (Ident name) _) env@(pc, sp, global, locals) = (newInstructions, newEnv)
    where newInstructions = generateFunctionInputCode p
          newIdent = (name, sp, Param flowMode changeMode)
          newEnv = (pc + length newInstructions, sp + 1, global, addToLocalScope locals newIdent)

generateFunctionInputCode :: IMLVal -> [Instruction]
generateFunctionInputCode (ParamDeclaration _ _ (Ident name) _) = [ loadIm32 0 ]

-- HERE THE LOCAL ENVIROMENT GETS UPDATED
generateScopeCode ::  [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
generateScopeCode statements startEnv = dropLocalScope $ generateMultiCode statements (addLocalScope [] startEnv)
    where dropLocalScope (instructions, (pc, sp, global, _ : locals)) = (instructions, (pc, sp, global, locals))

generateMultiCode :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
generateMultiCode instructions startEnv = foldl connectCode ([], startEnv) instructions

connectCode :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectCode (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateCode statement env

generateCode :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateCode (Ident name) env = ([ loadAddress $ getIdentAddress env name, deref ], updateCodeAddress env 2)
generateCode (Literal (IMLInt i)) env = ([loadIm32 $ toInteger i], updateCodeAddress env 1)
generateCode (MonadicOpr Parser.Minus expression) env = (expressionInstructions ++ [neg], updateCodeAddress newEnv 1)
    where (expressionInstructions, newEnv) = generateCode expression env
generateCode (Assignment (Ident name) expression) env = generateAssinmentCode name (thd3 $ getIdent env name) (generateCode expression env) --([loadAddress $ getIdentAddress env name] ++ expressionInstructions ++ [store], updateCodeAddress newEnviroment 2)
    --where (expressionInstructions, newEnviroment) = generateCode expression env
generateCode (IdentFactor ident Nothing) env = generateCode ident env
generateCode (DyadicOpr op a b) env = (expressionInstructions ++ [getDyadicOpr op], updateCodeAddress newEnv 1)
    where (expressionInstructions, newEnv) = (fst (generateCode a env) ++ fst (generateCode b env), snd $ generateCode b (snd $ generateCode a env))
generateCode (If condition ifStatements elseStatements) env@(_, _, global, locals) = (conditionInstructions ++ [condJump (getPc ifEndEnv + 1)] ++ ifStatementInstructions ++ [uncondJump (getPc elseEndEnv)] ++ elseStatementInstructions, elseEndEnv)
    where (conditionInstructions, condEndEnv) = generateCode condition env
          (ifStatementInstructions, ifEndEnv) = generateScopeCode ifStatements (updateCodeAddress condEndEnv 1) --TODO use the hole elseStament
          (elseStatementInstructions, elseEndEnv) = generateScopeCode elseStatements (updateCodeAddress ifEndEnv 1) --TODO use the hole elseStament
-- generateCode (FunctionCall name params) env = (prepParams ++ [ call $ getIdentAddress env name ], updateCodeAddress prepParamsEndEnv 1)
--    where (prepParams, prepParamsEndEnv) = generateMultiCode params env
generateCode s _ = error $ "not implemented" ++ show s

generateAssinmentCode :: String -> IdentInfo -> ([Instruction], Enviroment) -> ([Instruction], Enviroment)
generateAssinmentCode name (CodeGenerator.Var var@(ClampInt _ _) _) (exprInst, exprEnv) = ([loadInst] ++ exprInst ++ clampInst, updateCodeAddress clampEnv 1)
    where loadInst = loadAddress $ getIdentAddress exprEnv name
          (clampInst, clampEnv) = generateClampAssinmentCode loadInst var exprEnv
generateAssinmentCode name (CodeGenerator.Var var@(ArrayInt _ _) _) (exprInst, exprEnv) = error "TODO"
generateAssinmentCode name _ (exprInst, exprEnv)= ([loadAddress $ getIdentAddress exprEnv name] ++ exprInst ++ [store], updateCodeAddress exprEnv 2)

-- preconditon address is already loaded in the stack
generateClampAssinmentCode :: Instruction -> IMLType -> Enviroment -> ([Instruction], Enviroment)
generateClampAssinmentCode loadAddInst (ClampInt cmin cmax) env = (checkMaxInst ++ checkMinInst ++ storeInRangeInst ++ storeOverMax ++ storeUnderMin, updateCodeAddress env (afterAssinmentPc - 1))
    where startPc = getPc env
          checkMaxLength = 4
          checkMinLength = 4
          storeInRangeLength = 2
          storeOverMaxLenght = 5
          storeUnderMinLenght = 4
          afterAssinmentPc = startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeUnderMinLenght + 1
          checkMaxInst = [Dup, loadIm32 $ toInteger cmax, le32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength)]
          checkMinInst = [Dup, loadIm32 $ toInteger cmin, gt32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeOverMaxLenght)]
          storeInRangeInst = [Store, uncondJump afterAssinmentPc]
          storeOverMax = [Store, loadAddInst, loadIm32 $ toInteger cmax, store, uncondJump afterAssinmentPc]
          storeUnderMin = [Store, loadAddInst, loadIm32 $ toInteger cmin, store]
generateClampAssinmentCode _ _ _ = error "Type is not a ClampInt"

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