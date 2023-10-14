module Main where

import Data.Primitive.ByteArray (byteArrayFromList)
import Data.Map (Map)
import qualified Data.Map

import Instruction
import Device
import Processor
import Assembler
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

newProcessor :: Processor
newProcessor = Processor (replicate numRegisters 0) defaultFlags $ Data.Map.fromList [
        device 0x0000 0xf000 $ ReadOnlyMemory $ byteArrayFromList [34 :: Int],
        device 0xfffe 0xffff StdIO
    ]
    where device lo hi d = ((lo, hi), GenericDevice d lo)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            case assemble filename contents of
                Left program -> do
                    print program
                    cpu <- runProgram newProcessor program
                    print cpu
                Right e -> do
                    print e
                    exitFailure
        _ -> do
            pname <- getProgName
            putStrLn $ "Usage: " ++ pname ++ " [filename]"
            exitFailure
   
