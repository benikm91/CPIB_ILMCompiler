module VMSample ( run ) where

import VirtualMachineIO
import Locations
import Data.Array
import Data.Monoid

import ErrorHandlingGeneric(Loc, ErrorMsgGen, CheckGen, CheckerGen)

run :: IO ()
run = do 
    t <- debugProgram ("HambbeToni", program)
    print t
    return ()

program :: (Array Int Instruction)
program = array (0, 3) [(0, LoadIm IntVmTy (IntVmVal 5)), (1, LoadIm IntVmTy (IntVmVal 4)), (2, Add IntVmTy (rc2loc (1, 1))), (3, Stop)]