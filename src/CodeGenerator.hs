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

type Address = Int

data IdentInfo = Param IMLType IMLFlowMode IMLChangeMode
               | Var IMLType IMLChangeMode
               | Function [Ident] -- parameters
               deriving (Show)

type Ident = (String, Address, IdentInfo)

type Scope = [Ident]

-- stack of scopes
type Environment = (CodeAddress, Address, Address, Scope, [Scope]) -- PC, FP, SP Global, Locals

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

-- Helper Functions

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 ::  (a, b, c) -> c
thd3 (_, _, c) = c

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not left"

extractImlType :: IdentInfo -> IMLType
extractImlType (Param imlType _ _) = imlType
extractImlType (CodeGenerator.Var imlType _) = imlType
extractImlType _ = error "cannot extract type of a function"

updatePc :: Environment -> Int -> Environment
updatePc (pc, fp, sp, global, locals) i = (pc + i, fp, sp, global, locals)

updateSp :: Environment -> Int -> Environment
updateSp (pc, fp, sp, global, locals) i = (pc, fp, sp + i, global, locals)

-- env pc sp 
updatePcSp :: Environment -> Int -> Int -> Environment
updatePcSp (pc, fp, sp, global, locals) i k = (pc + i, fp, sp + k, global, locals)

addLocalIdent :: Environment -> Ident -> SourcePos -> Environment
addLocalIdent env@(pc, fp, sp, global, locals) ident@(name, _, _) pos = case findIdent of
    Nothing -> (pc, fp, sp, global, addToLocalScope locals ident)
    Just _ -> error ("Identifier with name "++ name ++" already defined in scope" ++ printLine pos ++ "\n")
    where findIdent = getIdentMaybe env name

addLocalScope :: Scope -> Environment -> Environment 
addLocalScope scope (pc, fp, sp, global, locals) = (pc, fp, sp, global, [scope] ++ locals)  

dropLocalScope :: Environment -> Environment
dropLocalScope (pc, fp, sp, global, toRemove : rest) = (pc, fp, sp - (length toRemove), global, rest)   

addToLocalScope :: [Scope] -> Ident -> [Scope]
addToLocalScope (next : rest) ident = (ident : next) : rest 

addToGlobalScope :: Environment -> Ident -> Environment
addToGlobalScope (pc, fp, sp, global, locals) ident = (pc, fp, sp, ident : global, locals)

removeLocalScope :: Environment -> Environment
removeLocalScope (pc, fp, sp, global, locals)  = (pc, fp, sp, global, tail locals)

getPc :: Environment -> Int
getPc (pc, _, _, _, _) = pc

getSp :: Environment -> Address
getSp (_, _, sp, _, _) = sp

addFp :: Environment -> Environment
addFp (pc, fp, sp, global, locals) = (pc, sp, 3, global, locals)

removeFp :: Environment -> Environment
removeFp (pc, fp, sp, global, locals) = (pc, 0, fp, global, locals) 

getLocalScopes :: Environment -> [Scope]
getLocalScopes (_, _, _, _, locals) = locals 

findInScope :: Scope -> String -> Maybe Ident
findInScope [] _ = Nothing
findInScope (next : rest) name = if (fst3 next) == name then Just next else findInScope rest name 

getIdent :: Environment -> String -> SourcePos -> Ident
getIdent (_, _, _, global, []) name pos = case findInScope global name of 
    Nothing -> error $ "Identifier " ++ name ++ " not found!" ++ printLine pos ++ "\n"
    Just a -> a
getIdent (pc, fp, sp, global, next : rest) name pos = case findInScope next name of
    Nothing -> getIdent (pc, fp, sp, global, rest) name pos
    Just a -> a

getIdentMaybe :: Environment -> String -> Maybe Ident
getIdentMaybe (_, _, _, global, []) name = findInScope global name
getIdentMaybe (pc, fp, sp, global, next : rest) name = case findInScope next name of
    Nothing -> getIdentMaybe (pc, fp, sp, global, rest) name
    Just a -> Just a

getIdentAddress :: Environment -> String -> SourcePos -> Address
getIdentAddress a b pos = snd3 (getIdent a b pos)

getIdentInfo :: Environment -> String -> SourcePos -> IdentInfo
getIdentInfo a b pos = thd3 (getIdent a b pos)

getParams :: IdentInfo -> [Ident]
getParams (Function idents) = idents

toArray :: [a] -> Array Int a
toArray l = array (0, length l - 1)  (zip [0 .. length l - 1] l)

emptyEnv :: Environment
emptyEnv = (0, 0, 0, [], [])

printLine :: SourcePos -> String
printLine pos = " on line " ++ (show $ sourceLine pos) ++ " collum " ++ (show $ sourceColumn pos)

-- End Helper Functions

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

moveSpUp :: Int -> Instruction
moveSpUp i = MoveSpUp i

-- End Instruction

generateOutputs :: Environment -> ([Instruction], Environment)
generateOutputs env@(pc, fp, sp, global, [] : []) = ([], env)
-- Array
generateOutputs env@(pc, fp, sp, global, ((name, addr, Param (ArrayInt amin amax) Out   _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where arrayLength = amax - amin + 1
          (newInstructions, newEnv) = (concat $ map (\x -> generateOutputCode (addr + x) (name ++ "[" ++ (show x) ++ "]")) [0..(arrayLength -1)], (pc + (length newInstructions), fp, sp - arrayLength, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv 
generateOutputs env@(pc, fp, sp, global, ((name, addr, Param (ArrayInt amin amax) InOut   _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where arrayLength = amax - amin + 1
          (newInstructions, newEnv) = (concat $ map (\x -> generateOutputCode (addr + x) (name ++ "[" ++ (show x) ++ "]")) [0..(arrayLength -1)], (pc + (length newInstructions), fp, sp - arrayLength, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv
-- Rest
generateOutputs env@(pc, fp, sp, global, ((name, addr, Param _ Out   _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where (newInstructions, newEnv) = (generateOutputCode addr name, (pc + (length newInstructions), fp, sp - 1, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv
generateOutputs env@(pc, fp, sp, global, ((name, addr, Param _ InOut _) : rest) : []) = (restInstructions ++ newInstructions, finalEnv)
    where (newInstructions, newEnv) = (generateOutputCode addr name, (pc + (length newInstructions), fp, sp - 1, global, [rest]))
          (restInstructions, finalEnv) = generateOutputs newEnv
generateOutputs env@(pc, fp, sp, global, ((name, addr, Param _ _     _) : rest) : []) = generateOutputs (pc, fp, sp - 1, global, [rest])

generateOutputCode :: Address -> String -> [Instruction]
generateOutputCode addr name = [loadAddress addr, deref, output32 name]

generateInputs :: [IMLVal] -> Environment -> ([Instruction], Environment)
generateInputs statements startEnv = foldl connectInput ([], addLocalScope [] startEnv) statements

connectInput :: ([Instruction], Environment) -> IMLVal -> ([Instruction], Environment)
connectInput (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateInput statement env

generateInput :: IMLVal -> Environment -> ([Instruction], Environment)
generateInput p env = (newInstructions, newEnv)
    where (newInstructions, newEnv) = generateInputCode p env

generateInputCode :: IMLVal -> Environment -> ([Instruction], Environment)
-- Int
generateInputCode par@(ParamDeclaration imlFlowMode changeMode (Ident name pos) Int _) env = ([instruction], updatePcSp newEnv 1 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Param Int imlFlowMode changeMode) pos
          instruction = getLoadInputInstruction imlFlowMode 0 name pos
-- ClampInt
generateInputCode par@(ParamDeclaration imlFlowMode changeMode (Ident name identPos) var@(ClampInt cmin cmax) pos) env
    | cmax < cmin = error ("Max of Clamp must be greater or equal than min" ++ printLine pos ++ " | " ++ show var ++ "\n")
    | otherwise = ([instruction] ++ checkMinInst ++ checkMaxInst ++ setToMinInst ++ setToMaxInst, updatePcSp newEnv 15 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Param var imlFlowMode changeMode) identPos
          instruction = getLoadInputInstruction imlFlowMode cmin name pos
          startPc = getPc env
          setToMinPc = startPc + 10
          setToMaxPc = setToMinPc + 3
          endPc = setToMaxPc + 2
          checkMinInst = [Dup, loadIm32 $ toInteger cmin, ge32, condJump setToMinPc]
          checkMaxInst = [Dup, loadIm32 $ toInteger cmax, le32, condJump setToMaxPc, uncondJump endPc]
          setToMinInst = [moveSpUp 1, loadIm32 $ toInteger cmin, uncondJump endPc]
          setToMaxInst = [moveSpUp 1, loadIm32 $ toInteger cmax]

-- Array
generateInputCode par@(ParamDeclaration imlFlowMode changeMode (Ident name identPos) var@(ArrayInt amin amax) pos) env
    | amax < amin = error ("Max of Array must be greater or equal than min" ++ printLine pos ++ " | " ++ show var ++ "\n")
    | otherwise = (instructions, updatePcSp newEnv (length instructions) arrayLength)
    where arrayLength = amax - amin + 1;
          newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Param var imlFlowMode changeMode) identPos
          instructions = concat $ map (\x -> [ (getLoadInputInstruction imlFlowMode 0 (name ++ "[" ++ show x ++ "]") pos) ]) [0.. (arrayLength - 1)] 

getLoadInputInstruction :: IMLFlowMode -> Int -> String -> SourcePos -> Instruction
getLoadInputInstruction Out i _    _   = loadIm32 $ toInteger i
getLoadInputInstruction _   _ name pos = input32 name pos

generateFunctions :: [IMLVal] -> Environment ->  ([Instruction], Environment)
generateFunctions [] env = ([], env)
generateFunctions statements startEnv = (instructions, newEnv)
    where (instructions, newEnv) = foldl connectFunction ([], startEnv) statements

connectFunction :: ([Instruction], Environment) -> IMLVal -> ([Instruction], Environment)
connectFunction (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateFunction statement env

generateFunction :: IMLVal -> Environment -> ([Instruction], Environment)
generateFunction (FunctionDeclaration (Ident name _) params statements _) env = (instructions, removeLocalScope $ removeFp newEnv)
    where (paramInstructions, inputEndEnv) = generateFunctionInputs (reverse params) (addLocalScope [] $ addFp env)
          -- add the function to the global scope so the function already knows itself (for recursion)
          inputScope = (head . getLocalScopes) inputEndEnv
          newIdent = (name, getPc env, Function inputScope)
          (statementInstructions, functionEndEnv) = generateMultiCode statements $ addToGlobalScope inputEndEnv newIdent 
          (returnInstruction, returnEndEnv) = ([ Return 0 ], updatePc functionEndEnv 1)
          instructions = paramInstructions ++ statementInstructions ++ returnInstruction
          newEnv = returnEndEnv
    
generateFunctionInputs :: [IMLVal] -> Environment -> ([Instruction], Environment)
generateFunctionInputs statements startEnv = foldl connectFunctionInput ([], startEnv) statements

connectFunctionInput :: ([Instruction], Environment) -> IMLVal -> ([Instruction], Environment)
connectFunctionInput (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateFunctionInput statement env

generateFunctionInput :: IMLVal -> Environment -> ([Instruction], Environment)
generateFunctionInput p@(ParamDeclaration flowMode changeMode (Ident name _) imlType _) env@(pc, fp, sp, global, locals) = (newInstructions, newEnv)
    where newInstructions = generateFunctionInputCode p
          newIdent = (name, - (1 + length ((head . getLocalScopes) env)), Param imlType flowMode changeMode)
          newEnv = (pc + length newInstructions, fp, sp, global, addToLocalScope locals newIdent)

generateFunctionInputCode :: IMLVal -> [Instruction]
generateFunctionInputCode (ParamDeclaration flowMode _ (Ident name _) _ _) = [ ]


generateScopeCode ::  [IMLVal] -> Environment -> ([Instruction], Environment)
generateScopeCode statements startEnv = (newInstructions, newEnv)
    where (scopeInstructions, scopeEndEnv) = generateMultiCode statements (addLocalScope [] startEnv)
          (dropScopeInstructions, dropScopeEndEnv) = ([ MoveSpUp (length $ (head . getLocalScopes) scopeEndEnv) ], updatePc (dropLocalScope scopeEndEnv) 1)
          newInstructions = scopeInstructions ++ dropScopeInstructions
          newEnv = dropScopeEndEnv       

generateMultiCode :: [IMLVal] -> Environment -> ([Instruction], Environment)
generateMultiCode instructions startEnv = foldl connectCode ([], startEnv) instructions

connectCode :: ([Instruction], Environment) -> IMLVal -> ([Instruction], Environment)
connectCode (instructions, env) statement = (instructions ++ newInstructions, newEnv)
    where (newInstructions, newEnv) = generateCode statement env

generateCode :: IMLVal -> Environment -> ([Instruction], Environment)
-- Ident
generateCode (Ident name pos) env = ([loadAddrRel $ getIdentAddress env name pos, deref ], updatePcSp env 2 1)
-- array deref
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
generateCode (If condition ifStatements elseStatements _) env@(_, _, _, global, locals) = (condInstructions ++ branchInstructions ++ ifStatementInstructions ++ jumpInstructions ++ elseStatementInstructions, newEnv)
    where (condInstructions, condEndEnv) = generateCode condition env
          (branchInstructions, ifStartEnv) = ([condJump (getPc elseStartEnv)], updatePcSp condEndEnv 1 (-1))
          (ifStatementInstructions, ifEndEnv) = generateScopeCode ifStatements ifStartEnv
          (jumpInstructions, elseStartEnv) = ([uncondJump (getPc newEnv)], updatePc ifEndEnv 1)
          (elseStatementInstructions, elseEndEnv) = generateScopeCode elseStatements elseStartEnv
          newEnv = elseEndEnv
-- Function call
generateCode (FunctionCall (Ident name pos) params _) env = (prepParams ++ callInstructions ++ storeOutputs ++ moveSpInVM, newEnv)
    where (prepParams, prepParamsEndEnv) = generateMultiCode params env
          (callInstructions, callEndEnv) = ([ call $ getIdentAddress env name pos], updatePc prepParamsEndEnv 1)
          (storeOutputs, storeOutputsEndEnv) = generateStoreOutputsCode (reverse (zip (params) (getParams $ getIdentInfo env name pos))) callEndEnv
          (moveSpInVM, moveSpInVMEndEnv) = ([MoveSpUp (length (params))], updatePc storeOutputsEndEnv 1)
          newEnv = moveSpInVMEndEnv
-- While
generateCode (While condition statements _) env@(_, _, _, global, locals) =  (condInstructions ++ leaveInstructions ++ statementInstructions ++ goBackInstructions, newEnv)
    where (condInstructions, condEndEnv) = generateCode condition env
          (leaveInstructions, leaveEndEnv) = ([condJump (getPc newEnv)], updatePcSp condEndEnv 1 (-1))
          (statementInstructions, statementsEndEnv) = generateScopeCode statements leaveEndEnv
          (goBackInstructions, goBackEndEnv) = ([uncondJump (getPc env)], updatePc statementsEndEnv 1)
          newEnv = goBackEndEnv
-- IdentDeclaration
generateCode (IdentDeclaration changeMode (Ident name _) imlType pos) env = generateIdentDeclarationCode name changeMode imlType env pos
-- For
generateCode (For (IdentFactor (Ident name identPos) Nothing _) statements pos) env = generateForCode (getIdent env name identPos) statements env pos
generateCode (For _ _ pos) env = error ("For only accepts an clamp Int" ++ printLine pos ++ "\n")
-- others
generateCode s _ = error $ "not implemented" ++ show s


generateForCode :: Ident -> [IMLVal] -> Environment -> SourcePos -> ([Instruction], Environment)
generateForCode ident@(name, addr, (CodeGenerator.Var (ClampInt cmin cmax) _)) statements env pos = (statementInstructions ++ condInstructions ++ incInstructions ++ leaveInstructions, newEnv)
    where (statementInstructions, statementsEndEnv) = generateScopeCode statements env
          (condInstructions, condEndEnv) = generateCode (DyadicOpr Parser.Eq (IdentFactor (Ident name pos) Nothing pos) (Literal (IMLInt cmax) pos) pos) statementsEndEnv
          (incInstructions, incEndEnv) = generateMultiCode [(Assignment (Ident name pos) (DyadicOpr Parser.Plus (IdentFactor (Ident name pos) Nothing pos) (Literal (IMLInt 1) pos) pos) pos)] condEndEnv
          (leaveInstructions, leaveEndEnv) = ([condJump (getPc env)], updatePcSp incEndEnv 1 (-1))
          newEnv = leaveEndEnv
generateForCode _ _ _ pos = error ("For only accepts an clamp Int" ++ printLine pos ++ "\n")

generateStoreOutputsCode :: [(IMLVal, Ident)] -> Environment -> ([Instruction], Environment)
generateStoreOutputsCode [] env = ([], env)
generateStoreOutputsCode (next : rest) env = (newInstructions ++ restInstructions, finalEnv)
    where (newInstructions, newEnv) = handleNext next env
          (restInstructions, finalEnv) = generateStoreOutputsCode rest newEnv

handleNext :: (IMLVal, Ident) -> Environment -> ([Instruction], Environment)
handleNext (IdentFactor (Ident name pos) _ _, (_, _, Param _ Out   changeMode)) env@(_, fp, sp, _, _) = ([ loadAddrRel $ getIdentAddress env name pos, loadAddrRel $ sp - 1, deref, store ], updatePcSp env 4 (-1))
handleNext (IdentFactor (Ident name pos) _ _, (_, _, Param _ InOut changeMode)) env@(_, fp, sp, _, _) = ([ loadAddrRel $ getIdentAddress env name pos, loadAddrRel $ sp - 1, deref, store ], updatePcSp env 4 (-1))
handleNext _ env@(_, _, sp, _, _) = ([], updateSp env (-1))

generateIdentDeclarationCode :: String -> IMLChangeMode -> IMLType -> Environment -> SourcePos -> ([Instruction], Environment)
generateIdentDeclarationCode name changeMode Int env pos = ([loadIm32 0], updatePcSp newEnv 1 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var Int changeMode) pos
generateIdentDeclarationCode name changeMode var@(ClampInt cmin cmax) env pos
    | cmax < cmin = error ("Max of Clamp must be greater or equal than min" ++ printLine pos ++ "\n")
    | otherwise = ([loadIm32 $ toInteger cmin], updatePcSp newEnv 1 1)
    where newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var var changeMode) pos
generateIdentDeclarationCode name changeMode var@(ArrayInt amin amax) env pos
    | amax < amin = error ("Max of Array must be greater or equal than min" ++ printLine pos ++ "\n")
    | otherwise =  (instructions, updatePcSp newEnv (amax - amin + 1) (amax - amin + 1))
    where instructions = generateIdentDeclarationArrayCode amin amax
          newEnv = addLocalIdent env (name, getSp env, CodeGenerator.Var var changeMode) pos

generateIdentDeclarationArrayCode :: Int -> Int -> [Instruction]
generateIdentDeclarationArrayCode i amax
    | i > amax = []
    | otherwise = [loadIm32 0] ++ generateIdentDeclarationArrayCode (i+1) amax

generateAssignmentCode :: IMLVal -> IdentInfo -> ([Instruction], Environment) -> ([Instruction], Environment)
-- clamp assginment (var)
generateAssignmentCode (Ident name pos) (CodeGenerator.Var var@(ClampInt _ _) _) (exprInst, exprEnv) = ([loadInst] ++ exprInst ++ clampInst, updatePcSp clampEnv 1 (-1))
    where loadInst = loadAddress $ getIdentAddress exprEnv name pos
          (clampInst, clampEnv) = generateClampAssignmentCode var exprEnv
-- clamp assginment (param)
generateAssignmentCode (Ident name pos) (Param var@(ClampInt _ _) _ _) (exprInst, exprEnv) = ([loadInst] ++ exprInst ++ clampInst, updatePcSp clampEnv 1 (-1))
    where loadInst = loadAddress $ getIdentAddress exprEnv name pos
          (clampInst, clampEnv) = generateClampAssignmentCode var exprEnv
-- array assignment (var)
generateAssignmentCode (IdentArray (Ident name identPos) i identArrPos) (CodeGenerator.Var var@(ArrayInt amin amax) _) (exprInst, exprEnv) = (loadArrayInstr ++ exprInst ++ [store], updatePcSp loadArrayEnv 1 (-2))
    where (loadArrayInstr, loadArrayEnv) = loadArrayAddress (IdentArray (Ident name identPos) i identArrPos) exprEnv
-- array assignment (param)
generateAssignmentCode (IdentArray (Ident name identPos) i identArrPos) (CodeGenerator.Param var@(ArrayInt amin amax) _ _) (exprInst, exprEnv) = (loadArrayInstr ++ exprInst ++ [store], updatePcSp loadArrayEnv 1 (-2))
    where (loadArrayInstr, loadArrayEnv) = loadArrayAddress (IdentArray (Ident name identPos) i identArrPos) exprEnv
-- normal
generateAssignmentCode (Ident name pos) _ (exprInst, exprEnv)= ([loadAddrRel $ getIdentAddress exprEnv name pos] ++ exprInst ++ [store], updatePcSp exprEnv 2 (-1))
generateAssignmentCode t d e = error $ "Could not match . IMLVal: " ++ show t ++ " | identInfo: " ++ show d

-- preconditon address is already loaded in the stack
generateClampAssignmentCode :: IMLType -> Environment -> ([Instruction], Environment)
generateClampAssignmentCode (ClampInt cmin cmax) env = (checkMaxInst ++ checkMinInst ++ storeInRangeInst ++ storeOverMax ++ storeUnderMin, updatePc env (checkMaxLength + checkMinLength + storeInRangeLength + storeUnderMinLenght + storeOverMaxLenght))
    where startPc = getPc env
          checkMaxLength = 4
          checkMinLength = 4
          storeInRangeLength = 2
          storeOverMaxLenght = 4
          storeUnderMinLenght = 3
          afterAssignmentPc = startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeUnderMinLenght + storeOverMaxLenght + 1
          checkMaxInst = [Dup, loadIm32 $ toInteger cmax, le32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength + 1)]
          checkMinInst = [Dup, loadIm32 $ toInteger cmin, ge32, condJump (startPc + checkMaxLength + checkMinLength + storeInRangeLength + storeOverMaxLenght + 1)]
          storeInRangeInst = [store, uncondJump afterAssignmentPc]
          storeOverMax =  [moveSpUp 1, loadIm32 $ toInteger cmax, store, uncondJump afterAssignmentPc]
          storeUnderMin = [moveSpUp 1, loadIm32 $ toInteger cmin, store]
generateClampAssignmentCode _ _ = error "Type is not a ClampInt"

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

loadArrayAddress :: IMLVal -> Environment -> ([Instruction], Environment)
loadArrayAddress (IdentArray (Ident name pos) indexExpr arrayPos) env = (indexStatments ++ [loadIm32 $ toInteger startAddress, Add Int32VmTy $ rc2loc $ toRc arrayPos, loadIm32 $ toInteger amin, Sub Int32VmTy $ rc2loc $ toRc arrayPos, Convert Int32VmTy IntVmTy $ rc2loc $ toRc arrayPos], updatePc newEnv 5)
    where startAddress = getIdentAddress env name pos
          ident@(_, add, identInfo) = getIdent env name pos
          imlType = extractImlType identInfo
          amax = extractArrayMax imlType
          amin = extractArrayMin imlType
          (indexStatments, newEnv) = generateCode indexExpr env