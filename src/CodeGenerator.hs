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
import Text.ParserCombinators.Parsec hiding (spaces)

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

neg :: SourcePos -> Instruction
neg pos = Neg Int32VmTy $ rc2loc $ toRc pos

add32, sub32, mult32, divFloor32, modf :: SourcePos -> Instruction
add32      pos = Add Int32VmTy $ rc2loc (toRc pos)
sub32      pos = Sub Int32VmTy $ rc2loc(toRc pos)
mult32     pos = Mult Int32VmTy $ rc2loc(toRc pos)
divFloor32 pos = DivFloor Int32VmTy $ rc2loc(toRc pos)
modf       pos = VirtualMachineIO.ModFloor Int32VmTy $ rc2loc(toRc pos)
eq32, ne32, gt32, ge32, lt32, le32 :: Instruction
eq32 = VirtualMachineIO.Eq Int32VmTy
ne32 = VirtualMachineIO.Ne Int32VmTy
gt32 = VirtualMachineIO.Gt Int32VmTy
ge32 = VirtualMachineIO.Ge Int32VmTy
lt32 = VirtualMachineIO.Lt Int32VmTy
le32 = VirtualMachineIO.Le Int32VmTy

toRc :: SourcePos -> RC
toRc pos = (sourceLine pos, sourceColumn pos)

loadAddress :: Int -> Instruction
loadAddress addr = LoadIm IntVmTy (IntVmVal addr)

loadAddrRel :: Int -> Instruction
loadAddrRel = LoadAddrRel

loadIm32 :: Integer -> Instruction
loadIm32 val = LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 val))

input32 :: String -> SourcePos -> Instruction
input32 name pos = Input (IntTy 32) (rc2loc $ toRc pos) name

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
               | Function [Ident] -- parameters
               deriving (Show)

type Ident = (String, Address, IdentInfo)

type Scope = [Ident]

-- stack of scopes
type Enviroment = (CodeAddress, Address, Scope, [Scope]) -- PC, Global, Locals

extractImlType :: IdentInfo -> IMLType
extractImlType (Param imlType _ _) = imlType
extractImlType (CodeGenerator.Var imlType _) = imlType
extractImlType _ = error "cannot extract type of a function"

updatePc :: Enviroment -> Int -> Enviroment
updatePc (pc, sp, global, locals) i = (pc + i, sp, global, locals)

updateSp :: Enviroment -> Int -> Enviroment
updateSp (pc, sp, global, locals) i = (pc, sp + i, global, locals)

-- env pc sp 
updatePcSp :: Enviroment -> Int -> Int -> Enviroment
updatePcSp (pc, sp, global, locals) i k = (pc + i, sp + k, global, locals)

addLocalIdent :: Enviroment -> Ident -> SourcePos -> Enviroment
addLocalIdent env@(pc, sp, global, locals) ident@(name, _, _) pos = case findIdent of
    Nothing -> (pc, sp, global, addToLocalScope locals ident)
    Just _ -> error ("Identifier with name "++ name ++" already defined in scope" ++ printLine pos ++ "\n")
    where findIdent = getIdentMaybe env name

addLocalScope :: Scope -> Enviroment -> Enviroment 
addLocalScope scope (pc, sp, global, locals) = (pc, sp, global, [scope] ++ locals)  

addToLocalScope :: [Scope] -> Ident -> [Scope]
addToLocalScope (next : rest) ident = (ident : next) : rest 

addToGlobalScope :: Enviroment -> Ident -> Enviroment
addToGlobalScope (pc, sp, global, locals) ident = (pc, sp, ident : global, locals)

removeLocalScope :: Enviroment -> Enviroment
removeLocalScope (pc, sp, global, locals)  = (pc, sp, global, tail locals)

getPc :: Enviroment -> Int
getPc (pc, _, _, _) = pc

getSp :: Enviroment -> Address
getSp (_, sp, _, _) = sp

getLocalScopes :: Enviroment -> [Scope]
getLocalScopes (_, _, _, locals) = locals 

findInScope :: Scope -> String -> Maybe Ident
findInScope [] _ = Nothing
findInScope (next : rest) name = if (fst3 next) == name then Just next else findInScope rest name 

getIdent :: Enviroment -> String -> SourcePos -> Ident
getIdent (_, _, global, []) name pos = case findInScope global name of 
    Nothing -> error $ "Identifier " ++ name ++ " not found!" ++ printLine pos ++ "\n"
    Just a -> a
getIdent (pc, sp, global, next : rest) name pos = case findInScope next name of
    Nothing -> getIdent (pc, sp, global, rest) name pos
    Just a -> a

getIdentMaybe :: Enviroment -> String -> Maybe Ident
getIdentMaybe (_, _, global, []) name = findInScope global name
getIdentMaybe (pc, sp, global, next : rest) name = case findInScope next name of
    Nothing -> getIdentMaybe (pc, sp, global, rest) name
    Just a -> Just a

getIdentAddress :: Enviroment -> String -> SourcePos -> Address
getIdentAddress a b pos = snd3 (getIdent a b pos)

getIdentInfo :: Enviroment -> String -> SourcePos -> IdentInfo
getIdentInfo a b pos = thd3 (getIdent a b pos)

getParams :: IdentInfo -> [Ident]
getParams (Function idents) = idents

toArray :: [a] -> Array Int a
toArray l = array (0, length l - 1)  (zip [0 .. length l - 1] l)

emptyEnv :: Enviroment
emptyEnv = (0, 0, [], [])

printLine :: SourcePos -> String
printLine pos = " on line " ++ (show $ sourceLine pos) ++ " collum " ++ (show $ sourceColumn pos)

toHaskellVM :: IMLVal -> VMProgram
toHaskellVM (Program (Ident name _) params functions statements _) = (name, toArray codeArray)
    where codeArray = inputInstructions ++ callProgram ++ functionInstructions ++ statementInstructions ++ outputInstructions ++ stopInstructions
          (inputInstructions, inputEndEnv) = generateInputs params emptyEnv
          (functionInstructions, functionEndEnv) = generateFunctions functions (updatePc inputEndEnv 1)
          (callProgram, callProgramEndEnv) = ([ UncondJump $ getPc functionEndEnv ], functionEndEnv)
          (statementInstructions, statementEndEnv) = generateScopeCode statements callProgramEndEnv
          (outputInstructions, outputEndEnv) = generateOutputs statementEndEnv
          (stopInstructions, _) = ([Stop], updatePc outputEndEnv 1)
toHaskellVM _ = error "Input is not a Program \n"

generateOutputs :: Enviroment -> ([Instruction], Enviroment)
generateOutputs env@(pc, sp, global, [] : []) = ([], env)
-- Array
generateOutputs env@(pc, sp, global, ((name, addr, Param (ArrayInt amin amax) Out   _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where arrayLength = amax - amin + 1
          (newInstructions, newEnv) = (concat $ map (\x -> generateOutputCode (addr + x) (name ++ "[" ++ (show x) ++ "]")) [0..(arrayLength -1)], (pc + (length newInstructions), sp - arrayLength, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv 
generateOutputs env@(pc, sp, global, ((name, addr, Param (ArrayInt amin amax) InOut   _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where arrayLength = amax - amin + 1
          (newInstructions, newEnv) = (concat $ map (\x -> generateOutputCode (addr + x) (name ++ "[" ++ (show x) ++ "]")) [0..(arrayLength -1)], (pc + (length newInstructions), sp - arrayLength, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv
-- Rest
generateOutputs env@(pc, sp, global, ((name, addr, Param _ Out   _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where (newInstructions, newEnv) = (generateOutputCode addr name, (pc + (length newInstructions), sp - 1, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv
generateOutputs env@(pc, sp, global, ((name, addr, Param _ InOut _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where (newInstructions, newEnv) = (generateOutputCode addr name, (pc + (length newInstructions), sp - 1, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv
generateOutputs env@(pc, sp, global, ((name, addr, Param _ _     _) : rest) : []) = generateOutputs (pc, sp - 1, global, [rest])

generateOutputCode :: Address -> String -> [Instruction]
generateOutputCode addr name = [loadAddress addr, deref, output32 name]

generateInputs :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
generateInputs statements startEnv = foldl connectInput ([], addLocalScope [] startEnv) statements

-- TODO better name
connectInput :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectInput (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateInput statement env

generateInput :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateInput p env@(pc, sp, global, locals) = (newInstructions, newEnv)
    where (newInstructions, newEnv) = generateInputCode p env
        --   newIdent = (name, sp, Param imlType flowMode changeMode)
        --   newEnv = (pc + length newInstructions, sp + 1, global, addToLocalScope locals newIdent)

generateInputCode :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
-- Int
generateInputCode par@(ParamDeclaration imlFlowMode changeMode (Ident name pos) Int _) env = ([instruction], updatePcSp newEnv 1 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Param Int imlFlowMode changeMode) pos
          instruction = getLoadInputInstruction imlFlowMode 0 name pos
-- ClampInt
generateInputCode par@(ParamDeclaration imlFlowMode changeMode (Ident name identPos) var@(ClampInt cmin cmax) pos) env
    | cmax <= cmin = error ("Max of Clamp must be greater than min" ++ printLine pos ++ " | " ++ show var ++ "\n")
    | otherwise = ([instruction], updatePcSp newEnv 1 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Param var imlFlowMode changeMode) identPos
          instruction = getLoadInputInstruction imlFlowMode cmin name pos
-- Array
generateInputCode par@(ParamDeclaration imlFlowMode changeMode (Ident name identPos) var@(ArrayInt amin amax) pos) env
    | amax <= amin = error ("Max of Array must be greater than min" ++ printLine pos ++ " | " ++ show var ++ "\n")
    | otherwise = (instructions, updatePcSp newEnv (length instructions) arrayLength)
    where arrayLength = amax - amin + 1;
          newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Param var imlFlowMode changeMode) identPos
          instructions = concat $ map (\x -> [ (getLoadInputInstruction imlFlowMode 0 (name ++ "[" ++ show x ++ "]") pos) ]) [0.. (arrayLength - 1)] 

getLoadInputInstruction :: IMLFlowMode -> Int -> String -> SourcePos -> Instruction
getLoadInputInstruction Out i _    _   = loadIm32 $ toInteger i
getLoadInputInstruction _   _ name pos = input32 name pos

generateFunctions :: [IMLVal] -> Enviroment ->  ([Instruction], Enviroment)
generateFunctions [] env = ([], env)
generateFunctions statements startEnv = (instructions, newEnv)
    where (instructions, newEnv) = foldl connectFunction ([], startEnv) statements

connectFunction :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectFunction (instructions, env@(pc, _, _, _)) statement@(FunctionDeclaration (Ident name _) _ _ _) = (instructions ++ newInstructions, newEnv)
    where (newInstructions, functionEnv, inputScope) = generateFunction statement env
          newIdent = (name, pc, Function inputScope)
          -- add the function to the global scope and remove the last local scope (which is from the function)
          newEnv = addToGlobalScope functionEnv newIdent 

generateFunction :: IMLVal -> Enviroment -> ([Instruction], Enviroment, Scope)
generateFunction (FunctionDeclaration name params statements _) env = (instructions, removeLocalScope newEnv, (head . getLocalScopes) inputEndEnv)
    where (paramInstructions, inputEndEnv) = generateFunctionInputs (reverse params) (addLocalScope [] env)
          (statementInstructions, functionEndEnv) = generateMultiCode statements inputEndEnv 
          (returnInstruction, returnEndEnv) = ([ Return 0 ], updatePc functionEndEnv 1)
          instructions = paramInstructions ++ statementInstructions ++ returnInstruction
          newEnv = returnEndEnv
    
generateFunctionInputs :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
generateFunctionInputs statements startEnv = foldl connectFunctionInput ([], startEnv) statements

connectFunctionInput :: ([Instruction], Enviroment) -> IMLVal -> ([Instruction], Enviroment)
connectFunctionInput (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateFunctionInput statement env

generateFunctionInput :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
generateFunctionInput p@(ParamDeclaration flowMode changeMode (Ident name _) imlType _) env@(pc, sp, global, locals) = (newInstructions, newEnv)
    where newInstructions = generateFunctionInputCode p
          newIdent = (name, - (1 + length ((head . getLocalScopes) env)), Param imlType flowMode changeMode)
          newEnv = (pc + length newInstructions, sp, global, addToLocalScope locals newIdent)

generateFunctionInputCode :: IMLVal -> [Instruction]
generateFunctionInputCode (ParamDeclaration flowMode _ (Ident name _) _ _) = [ ]

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
-- Ident
generateCode (Ident name pos) env = ([loadAddrRel $ getIdentAddress env name pos, deref ], updatePcSp env 2 1)
-- array deref TODO: set correct location
generateCode (IdentArray (Ident name identPos) indexExpr identArrPos) env = (loadAddressInstr ++ [deref], updatePc loadAddressEnv 1)
    where (loadAddressInstr, loadAddressEnv) = loadArrayAddress (IdentArray (Ident name identPos) indexExpr identArrPos) env
generateCode (Literal (IMLInt i) _) env = ([loadIm32 $ toInteger i], updatePcSp env 1 1)
-- MonadicOpr
generateCode (MonadicOpr Parser.Plus expression _) env = generateCode expression env
generateCode (MonadicOpr Parser.Minus expression pos) env = (expressionInstructions ++ [neg pos], updatePc newEnv 1)
    where (expressionInstructions, newEnv) = generateCode expression env 
generateCode (MonadicOpr Parser.Not expression pos) env = (expressionInstructions ++ [Convert IntVmTy Int32VmTy $ rc2loc (toRc pos), loadIm32 1, add32 pos, loadIm32 2, ModFloor Int32VmTy $ rc2loc (toRc pos), Convert Int32VmTy IntVmTy $ rc2loc (toRc pos)], updatePc newEnv 6)
    where (expressionInstructions, newEnv) = generateCode expression env
-- Assignments
generateCode (Assignment imlIdent@(Ident name pos)                  expression _) env = generateAssignmentCode imlIdent (thd3 $ getIdent env name pos) (generateCode expression env)
generateCode (Assignment imlIdent@(IdentArray (Ident name pos) _ _) expression _) env = generateAssignmentCode imlIdent (thd3 $ getIdent env name pos) (generateCode expression env)
-- IdentFactor
generateCode (IdentFactor ident Nothing _) env = generateCode ident env
-- DyadicOpr
generateCode (DyadicOpr Parser.And a b pos) env = (expressionInstructions ++ [mult32 pos, loadIm32 0, gt32], updatePcSp newEnv 5 (-1))
    where (expressionInstructions, newEnv) = (fst (generateCode a env) ++ [Convert IntVmTy Int32VmTy $ rc2loc (toRc pos)] ++ fst (generateCode b env) ++ [Convert IntVmTy Int32VmTy $ rc2loc (toRc pos)], snd $ generateCode b (snd $ generateCode a env))
generateCode (DyadicOpr Parser.Or a b pos) env = (expressionInstructions ++ [add32 pos, loadIm32 0, gt32], updatePcSp newEnv 5 (-1))
    where (expressionInstructions, newEnv) = (fst (generateCode a env) ++ [Convert IntVmTy Int32VmTy $ rc2loc (toRc pos)] ++ fst (generateCode b env) ++ [Convert IntVmTy Int32VmTy $ rc2loc (toRc pos)], snd $ generateCode b (snd $ generateCode a env))
generateCode (DyadicOpr op a b pos) env = (expressionInstructions ++ [getDyadicOpr op pos], updatePcSp newEnv 1 (-1))
    where (expressionInstructions, newEnv) = (fst (generateCode a env) ++ fst (generateCode b env), snd $ generateCode b (snd $ generateCode a env))
-- If
generateCode (If condition ifStatements elseStatements _) env@(_, _, global, locals) = (condInstructions ++ branchInstructions ++ ifStatementInstructions ++ jumpInstructions ++ elseStatementInstructions, newEnv)
    where (condInstructions, condEndEnv) = generateCode condition env
          (branchInstructions, branchEndEnv) = ([condJump (getPc ifEndEnv + 1)], updatePcSp condEndEnv 1 (-1))
          (ifStatementInstructions, ifEndEnv) = generateScopeCode ifStatements branchEndEnv
          (jumpInstructions, jumpEndEnv) = ([uncondJump (getPc newEnv)], updatePc ifEndEnv 1)
          (elseStatementInstructions, elseEndEnv) = generateScopeCode elseStatements jumpEndEnv
          newEnv = elseEndEnv
-- Function call
generateCode (FunctionCall (Ident name pos) params _) env = (prepParams ++ callInstructions ++ storeOutputs, storeOutputsEndEnv)
    where (prepParams, prepParamsEndEnv) = generateMultiCode params env
          (callInstructions, callEndEnv) = ([ call $ getIdentAddress env name pos], updatePc prepParamsEndEnv 1)
          (storeOutputs, storeOutputsEndEnv) = generateStoreOutputsCode (reverse (zip (params) (getParams $ getIdentInfo env name pos))) callEndEnv
-- While
generateCode (While condition statements _) env@(_, _, global, locals) =  (condInstructions ++ leaveInstructions ++ statmentInstructions ++ goBackInstructions, newEnv)
    where (condInstructions, condEndEnv) = generateCode condition env
          (leaveInstructions, leaveEndEnv) = ([condJump (getPc newEnv)], updatePcSp condEndEnv 1 (-1))
          (statmentInstructions, statementsEndEnv) = generateScopeCode statements leaveEndEnv
          (goBackInstructions, goBackEndEnv) = ([uncondJump (getPc env)], updatePc statementsEndEnv 1)
          newEnv = goBackEndEnv
-- IdentDeclaration
generateCode (IdentDeclaration changeMode (Ident name _) imlType pos) env = generateIdentDeclarationCode name changeMode imlType env pos
-- For
generateCode (For (IdentFactor (Ident name identPos) Nothing _) statements pos) env = generateForCode (getIdent env name identPos) statements env pos
generateCode (For _ _ pos) env = error ("For only accepts an clamp Int" ++ printLine pos ++ "\n")
-- others
generateCode s _ = error $ "not implemented" ++ show s

generateForCode :: Ident -> [IMLVal] -> Enviroment -> SourcePos -> ([Instruction], Enviroment)
generateForCode ident@(name, addr, (CodeGenerator.Var (ClampInt cmin cmax) _)) statements env pos = generateCode (While newCondition newStatements pos) env
    where newCondition = (DyadicOpr Parser.Lt (IdentFactor (Ident name pos) Nothing pos) (Literal (IMLInt cmax) pos) pos)
          newStatements = statements ++ [(Assignment (Ident name pos) (DyadicOpr Parser.Plus (IdentFactor (Ident name pos) Nothing pos) (Literal (IMLInt 1) pos) pos) pos)]
generateForCode _ _ _ pos = error ("For only accepts an clamp Int" ++ printLine pos ++ "\n")

generateStoreOutputsCode :: [(IMLVal, Ident)] -> Enviroment -> ([Instruction], Enviroment)
generateStoreOutputsCode [] env = ([], env)
generateStoreOutputsCode (next : rest) env = (newInstructions ++ restInstructions, finalEnv)
    where (newInstructions, newEnv) = handleNext next env
          (restInstructions, finalEnv) = generateStoreOutputsCode rest newEnv

handleNext :: (IMLVal, Ident) -> Enviroment -> ([Instruction], Enviroment)
handleNext (IdentFactor (Ident name pos) _ _, (_, _, Param _ Out   changeMode)) env@(_, sp, _, _) = ([ loadAddrRel $ getIdentAddress env name pos, loadAddrRel $ sp - 1, deref, store ], updatePcSp env 4 (-1))
handleNext (IdentFactor (Ident name pos) _ _, (_, _, Param _ InOut changeMode)) env@(_, sp, _, _) = ([ loadAddrRel $ getIdentAddress env name pos, loadAddrRel $ sp - 1, deref, store ], updatePcSp env 4 (-1))
handleNext _ env = ([], updateSp env (-1))

generateIdentDeclarationCode :: String -> IMLChangeMode -> IMLType -> Enviroment -> SourcePos -> ([Instruction], Enviroment)
generateIdentDeclarationCode name changeMode Int env pos = ([loadIm32 0], updatePcSp newEnv 1 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var Int changeMode) pos
generateIdentDeclarationCode name changeMode var@(ClampInt cmin cmax) env pos
    | cmax <= cmin = error ("Max of Clamp must be greater than min" ++ printLine pos ++ "\n")
    | otherwise = ([loadIm32 $ toInteger cmin], updatePcSp newEnv 1 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var var changeMode) pos
generateIdentDeclarationCode name changeMode var@(ArrayInt amin amax) env pos
    | amax <= amin = error ("Max of Array must be greater than min" ++ printLine pos ++ "\n")
    | otherwise =  (instructions, updatePcSp newEnv (amax - amin + 1) (amax - amin + 1))
    where instructions = generateIdentDeclarationArrayCode amin amax
          newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var var changeMode) pos

generateIdentDeclarationArrayCode :: Int -> Int -> [Instruction]
generateIdentDeclarationArrayCode i amax
    | i > amax = []
    | otherwise = [loadIm32 0] ++ generateIdentDeclarationArrayCode (i+1) amax

generateAssignmentCode :: IMLVal -> IdentInfo -> ([Instruction], Enviroment) -> ([Instruction], Enviroment)
-- clamp assginment (var)
generateAssignmentCode (Ident name pos) (CodeGenerator.Var var@(ClampInt _ _) _) (exprInst, exprEnv) = ([loadInst] ++ exprInst ++ clampInst, updatePcSp clampEnv 1 (-1))
    where loadInst = loadAddress $ getIdentAddress exprEnv name pos
          (clampInst, clampEnv) = generateClampAssignmentCode loadInst var exprEnv
-- clamp assginment (param)
generateAssignmentCode (Ident name pos) (Param var@(ClampInt _ _) _ _) (exprInst, exprEnv) = ([loadInst] ++ exprInst ++ clampInst, updatePcSp clampEnv 1 (-1))
    where loadInst = loadAddress $ getIdentAddress exprEnv name pos
          (clampInst, clampEnv) = generateClampAssignmentCode loadInst var exprEnv
-- array assignment (var)
generateAssignmentCode (IdentArray (Ident name identPos) i identArrPos) (CodeGenerator.Var var@(ArrayInt amin amax) _) (exprInst, exprEnv) = (loadArrayInstr ++ exprInst ++ [store], updatePcSp loadArrayEnv 1 (-1))
    where (loadArrayInstr, loadArrayEnv) = loadArrayAddress (IdentArray (Ident name identPos) i identArrPos) exprEnv
-- array assignment (param)
generateAssignmentCode (IdentArray (Ident name identPos) i identArrPos) (CodeGenerator.Param var@(ArrayInt amin amax) _ _) (exprInst, exprEnv) = (loadArrayInstr ++ exprInst ++ [store], updatePcSp loadArrayEnv 1 (-1))
    where (loadArrayInstr, loadArrayEnv) = loadArrayAddress (IdentArray (Ident name identPos) i identArrPos) exprEnv
-- normal
generateAssignmentCode (Ident name pos) _ (exprInst, exprEnv)= ([loadAddrRel $ getIdentAddress exprEnv name pos] ++ exprInst ++ [store], updatePcSp exprEnv 2 (-1))
generateAssignmentCode t d e = error $ "Could not match . IMLVal: " ++ show t ++ " | identInfo: " ++ show d

-- preconditon address is already loaded in the stack
generateClampAssignmentCode :: Instruction -> IMLType -> Enviroment -> ([Instruction], Enviroment)
generateClampAssignmentCode loadAddInst (ClampInt cmin cmax) env = (checkMaxInst ++ checkMinInst ++ storeInRangeInst ++ storeOverMax ++ storeUnderMin, updatePc env (checkMaxLength + checkMinLength + storeInRangeLength + storeUnderMinLenght + storeOverMaxLenght))
    where startPc = getPc env
          checkMaxLength = 4
          checkMinLength = 4
          storeInRangeLength = 2
          storeOverMaxLenght = 5
          storeUnderMinLenght = 4
          afterAssignmentPc = startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeUnderMinLenght + storeOverMaxLenght + 1
          checkMaxInst = [Dup, loadIm32 $ toInteger cmax, le32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength + 1)]
          checkMinInst = [Dup, loadIm32 $ toInteger cmin, ge32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeOverMaxLenght + 1)]
          storeInRangeInst = [store, uncondJump afterAssignmentPc]
          storeOverMax = [store, loadAddInst, loadIm32 $ toInteger cmax, store, uncondJump afterAssignmentPc]
          storeUnderMin = [store, loadAddInst, loadIm32 $ toInteger cmin, store]
generateClampAssignmentCode _ _ _ = error "Type is not a ClampInt"

-- generateCodeWithNewScope :: [IMLVal] -> Enviroment -> ([Instruction], Enviroment)
-- generateCodeWithNewScope vals env = generateStatmensCode vals (addNewLocalScope env) []

-- generateStatmensCode :: [IMLVal] -> Enviroment -> [Instruction] -> ([Instruction], Enviroment)
-- generateStatmensCode [] env intructions = (intructions, removeLocalScope env)
-- generateStatmensCode (val:rest) env intructions = generateStatmensCode rest newEnv (intructions ++ newInstructions)
--     where (newInstructions, newEnv) = generateCode val env

getDyadicOpr :: IMLOperation -> SourcePos -> Instruction
getDyadicOpr Parser.Plus  pos = add32 pos
getDyadicOpr Parser.Minus pos = sub32 pos
getDyadicOpr Parser.Times pos = mult32 pos
getDyadicOpr Parser.Div   pos = divFloor32 pos
getDyadicOpr Parser.Mod   pos = modf pos
getDyadicOpr Parser.Lt    _   = lt32
getDyadicOpr Parser.Ge    _   = ge32
getDyadicOpr Parser.Eq    _   = eq32
getDyadicOpr Parser.Ne    _   = ne32
getDyadicOpr Parser.Gt    _   = gt32
getDyadicOpr Parser.Le    _   = le32

extractArrayMin :: IMLType -> Int
extractArrayMin (ArrayInt amin _) = amin

extractArrayMax :: IMLType -> Int
extractArrayMax (ArrayInt _ amax) = amax

loadArrayAddress :: IMLVal -> Enviroment -> ([Instruction], Enviroment)
loadArrayAddress (IdentArray (Ident name pos) indexExpr arrayPos) env = (indexStatments ++ [loadIm32 $ toInteger startAddress, Add Int32VmTy $ rc2loc $ toRc arrayPos, loadIm32 $ toInteger  amin, Sub Int32VmTy $ rc2loc $ toRc arrayPos, Convert Int32VmTy IntVmTy $ rc2loc $ toRc arrayPos], updatePcSp newEnv 5 1)
    where startAddress = getIdentAddress env name pos
          ident@(_, add, identInfo) = getIdent env name pos
          imlType = extractImlType identInfo
          amax = extractArrayMax imlType
          amin = extractArrayMin imlType
          (indexStatments, newEnv) = generateCode indexExpr env