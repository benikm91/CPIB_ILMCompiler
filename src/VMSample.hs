module VMSample ( run ) where

import BaseDecls
import VirtualMachineIO
import Locations
import Data.Array
import Data.Monoid
import CheckedArithmetic

import ErrorHandlingGeneric(Loc, ErrorMsgGen, CheckGen, CheckerGen)

run :: IO ()
run = do 
    t <- execProgram ("HambbeToni", program)
    print t
    return ()

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Not left"

program :: (Array Int Instruction)
program = array (0, 4) [(0, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 5))), (1, LoadIm Int32VmTy (Int32VmVal (fromRight $ fromIntegerToInt32 4))), (2, Add Int32VmTy mempty), (3, Output (IntTy (32 :: Int)) "HALLO"), (4, Stop)]