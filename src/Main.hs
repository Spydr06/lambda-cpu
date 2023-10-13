module Main where

import Data.Primitive.ByteArray (byteArrayFromList)
import Data.Map (Map)
import qualified Data.Map

import Instruction
import Device
import Processor

newProcessor :: Processor
newProcessor = Processor (replicate numRegisters 0) defaultFlags $ Data.Map.fromList [
        device 0x0000 0xf000 $ ReadOnlyMemory $ byteArrayFromList [34 :: Int],
        device 0xfffe 0xffff StdIO
    ]
    where device lo hi d = ((lo, hi), GenericDevice d lo)

main :: IO ()
main = do
    result <- runProgram newProcessor $ Program [
            Instruction Mov [Register X0, Literal 65],
            Instruction Mov [Address 0xfffe, Literal 1, Register X0],
            Instruction Add [Register X0, Literal 1],
            Instruction Cmp [Register X0, Literal 91],
            Instruction Jnz [Literal 1],
            Instruction Hlt []
        ]
    putChar '\n'
    print result
