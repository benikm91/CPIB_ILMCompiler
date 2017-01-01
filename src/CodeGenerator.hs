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

data IdentInfo = Param IMLType IMLFlowMode IMLChangeMode
               | Var IMLType IMLChangeMode
               | Function [IdentInfo]

type Ident = (String, Address, IdentInfo)

type Scope = [Ident]

-- stack of scopes
type Enviroment = (CodeAddress, Address, Scope, [Scope]) -- PC, Global, Locals

updateCodeAddress :: Enviroment -> Int -> Enviroment
updateCodeAddress (pc, sp, global, locals) i = (pc + i, sp, global, locals)

addLocalIdent :: Enviroment -> Ident -> Enviroment
-- TODO Check if Ident already exists => Throw error
addLocalIdent (pc, sp, global, locals) ident = (pc, sp + 1, global, addToLocalScope locals ident)

addToLocalScope :: [Scope] -> Ident -> [Scope]
addToLocalScope (next : rest) ident = (ident : next) : rest 

addLocalScope :: Scope -> Enviroment -> Enviroment 
addLocalScope scope (pc, sp, global, locals) = (pc, sp, global, scope : locals)  

removeLocalScope :: Enviroment -> Enviroment
removeLocalScope (pc, sp, global, locals)  = (pc, sp, global, init locals)

getPc :: Enviroment -> Int
getPc (pc, _, _, _) = pc

getSp :: Enviroment -> Address
getSp (_, sp, _, _) = sp

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
generateOutputs ((name, addr, Param _ Out _) : rest) = [loadAddress addr, deref, output32 name] ++ generateOutputs rest
generateOutputs ((name, addr, Param _ InOut _) : rest) = [loadAddress addr, deref, output32 name] ++ generateOutputs rest
generateOutputs (_ : rest) = generateOutputs rest

generateInputs :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
generateInputs instructions startEnv = foldl connectInput ([], addLocalScope [] startEnv) instructions

-- TODO better name
connectInput :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectInput (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateInput statement env

generateInput :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateInput p@(ParamDeclaration flowMode changeMode (Ident name) imlType) env@(pc, sp, global, locals) = (newInstructions, newEnv)
    where newInstructions = generateInputCode p
          newIdent = (name, sp, Param imlType flowMode changeMode)
          newEnv = (pc + length newInstructions, sp + 1, global, addToLocalScope locals newIdent)

generateInputCode :: IMLVal -> [Instruction]
generateInputCode (ParamDeclaration Out  _ (Ident name) _) = [ loadIm32 0 ]
generateInputCode (ParamDeclaration _    _ (Ident name) _) = [ input32 name ]

generateFunctions :: [IMLVal] -> Enviroment ->  ([Instruction], Enviroment)
generateFunctions [] env = ([], env)
{-generateFunctions statements startEnv = (instructions, newEnv)
    where (instructions, newEnv) = foldl connectFunction ([], startEnv) statements

connectFunction :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectFunction (instructions, env) statement = (instructions ++ newInstructions, newEnv))
    where (newInstructions, newEnv) = generateFunction statement env 

generateFunction :: IMLVal -> Enviroment -> ([Instruction], [IdentInfo])
generateFunction (FunctionDeclaration name params statements) env@(pc, sp, global, locals) = (instructions, parameters)
    where (paramInstructions, inputEndEnv) = generateFunctionInputs (reverse params) sp
          (statementInstructions, functionEndEnv) = generateMultiCode inputEndEnv statements
          (returnInstruction, returnEndEnv) = (Return 0, updateCodeAddress functionEndEnv 1)
          instructions = paramInstructions ++ statementInstructions ++ returnInstruction
          newEnv = returnEndEnd -- (pc + length instructions, sp + length parameters, global, locals)
          
generateFunctionInputs :: [IMLVal] -> Address -> ([Instruction], [IdentInfo])
generateFunctionInputs instructions sp = foldl connectFunctionInput ([], (sp, []), []) instructions

connectFunctionInput :: ([Instruction], Scope, [IdentInfo]) -> IMLVal -> ([Instruction], Scope, [IdentInfo])
connectFunctionInput (instructions, (sp, scope), info) val = (instructions ++ newInstructions, (sp + 1, newIdent : scope, newInfo : info))
    where (newInstructions, newIdent, newInfo) = generateFunctionInput offset val sp

generateFunctionInput :: IMLVal -> Address -> ([Instruction], Ident, IdentInfo)
generateFunctionInput (ParamDeclaration flowMode changeMode (Ident name) _) sp = ([ loadIm32 0 ], (name, sp, identInfo), identInfo)
    where identInfo = Param flowMode changeMode

-}

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
generateCode (Assignment (Ident name) expression) env = generateAssignmentCode name (thd3 $ getIdent env name) (generateCode expression env) --([loadAddress $ getIdentAddress env name] ++ expressionInstructions ++ [store], updateCodeAddress newEnviroment 2)
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
generateCode (IdentDeclaration changeMode (Ident name) imlType) env = generateIdentDeclarationCode name changeMode imlType env
generateCode s _ = error $ "not implemented" ++ show s

--data IMLType = Int | ClampInt Int Int | ArrayInt Int Int -- from to

-- data IdentInfo = Param IMLType IMLFlowMode IMLChangeMode
--                | Var IMLType IMLChangeMode
--                | Function [IdentInfo]

-- type Ident = (String, Address, IdentInfo)


generateIdentDeclarationCode :: String -> IMLChangeMode -> IMLType -> Enviroment -> ([Instruction], Enviroment)
generateIdentDeclarationCode name changeMode Int env = ([loadIm32 0], updateCodeAddress newEnv 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var Int changeMode)
generateIdentDeclarationCode name changeMode var@(ClampInt cmin cmax) env 
    | cmax <= cmin = error "Max of Clamp muste be greater than min"
    | otherwise = ([loadIm32 $ toInteger cmin], updateCodeAddress newEnv 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var var changeMode)
generateIdentDeclarationCode name changeMode (ArrayInt amin amax) env = error "TODO"

-- generateIdentDeclarationArrayCode :: Int -> Enviroment -> ([Instruction], Enviroment)
-- generateIdentDeclarationArrayCode 

generateAssignmentCode :: String -> IdentInfo -> ([Instruction], Enviroment) -> ([Instruction], Enviroment)
generateAssignmentCode name (CodeGenerator.Var var@(ClampInt _ _) _) (exprInst, exprEnv) = ([loadInst] ++ exprInst ++ clampInst, updateCodeAddress clampEnv 1)
    where loadInst = loadAddress $ getIdentAddress exprEnv name
          (clampInst, clampEnv) = generateClampAssignmentCode loadInst var exprEnv
generateAssignmentCode name (Param var@(ClampInt _ _) _ _) (exprInst, exprEnv) = ([loadInst] ++ exprInst ++ clampInst, updateCodeAddress clampEnv 1)
    where loadInst = loadAddress $ getIdentAddress exprEnv name
          (clampInst, clampEnv) = generateClampAssignmentCode loadInst var exprEnv
generateAssignmentCode name (CodeGenerator.Var var@(ArrayInt _ _) _) (exprInst, exprEnv) = error "TODO"
generateAssignmentCode name _ (exprInst, exprEnv)= ([loadAddress $ getIdentAddress exprEnv name] ++ exprInst ++ [store], updateCodeAddress exprEnv 2)

-- preconditon address is already loaded in the stack
generateClampAssignmentCode :: Instruction -> IMLType -> Enviroment -> ([Instruction], Enviroment)
generateClampAssignmentCode loadAddInst (ClampInt cmin cmax) env = (checkMaxInst ++ checkMinInst ++ storeInRangeInst ++ storeOverMax ++ storeUnderMin, updateCodeAddress env (afterAssignmentPc - 1))
    where startPc = getPc env
          checkMaxLength = 4
          checkMinLength = 4
          storeInRangeLength = 2
          storeOverMaxLenght = 5
          storeUnderMinLenght = 4
          afterAssignmentPc = startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeUnderMinLenght + storeOverMaxLenght + 1
          checkMaxInst = [Dup, loadIm32 $ toInteger cmax, le32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength + 1)]
          checkMinInst = [Dup, loadIm32 $ toInteger cmin, gt32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeOverMaxLenght + 1)]
          storeInRangeInst = [Store, uncondJump afterAssignmentPc]
          storeOverMax = [Store, loadAddInst, loadIm32 $ toInteger cmax, store, uncondJump afterAssignmentPc]
          storeUnderMin = [Store, loadAddInst, loadIm32 $ toInteger cmin, store]
generateClampAssignmentCode _ _ _ = error "Type is not a ClampInt"

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