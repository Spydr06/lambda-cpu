{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Char (ord, chr)
import Data.Primitive.ByteArray
import Data.Map (Map)
import qualified Data.Map
import Control.Monad.Primitive (PrimMonad(PrimState))
import Control.Monad

data Mnemonic = Nop
    | Mov
    | Add
    | Sub
    deriving (Show, Eq)

data Register = X0
    | X1
    | X2
    | X3
    deriving (Show, Enum)

data Argument = Register Register
    | Address Int
    | Literal Int
    deriving Show

data Instruction = Instruction Mnemonic [Argument]
    deriving Show

newtype Program = Program [Instruction] 

data Processor = Processor {
    registers :: [Int],
    devices :: Map (Int, Int) GenericDevice
}

instance Show Processor where
    show = foldl (++) "registers:\n" . zipWith showRegister [0..] . registers
        where showRegister i r = "X" ++ show i ++ ":\t" ++ show r ++ "\n"

class Device a where
    readByte :: a -> Int -> IO Int
    writeByte :: a -> Int -> Int -> IO ()

data GenericDevice = forall a. Device a => GenericDevice a

instance Device GenericDevice where
    readByte (GenericDevice a) = readByte a
    writeByte (GenericDevice a) = writeByte a

data Memory a = Memory {
    bytes :: MutableByteArray a,
    size :: Int
} 

newtype StdIO = StdIO ()
instance Device StdIO where
    readByte _ _ = ord <$> getChar
    writeByte _ _ = putChar . chr 

numRegisters :: Int
numRegisters = 4

newProcessor :: Processor
newProcessor = Processor (replicate numRegisters 0) $ Data.Map.fromList [
        ((0xfffe, 0xffff), GenericDevice $ StdIO ())
    ]

main :: IO ()
main = do
    result <- runProgram newProcessor $ Program [
            Instruction Mov [Register X0, Literal 34],
            Instruction Mov [Register X1, Literal 35],
            Instruction Add [Register X0, Register X1],
            Instruction Mov [Address 0xfffe, Literal 1, Register X0]
        ]
    putChar '\n'
    print result

runProgram :: Processor -> Program -> IO Processor
runProgram cpu (Program instr) = foldl nextInstruction (return cpu) instr
    where nextInstruction cpu i = do
            c <- cpu
            runInstruction c i

runInstruction :: Processor -> Instruction -> IO Processor

-- Mov
runInstruction cpu (Instruction Mov [Register r, Literal l]) = return $ writeRegister cpu r l
runInstruction cpu (Instruction Mov [Register a, Register b]) = return $ writeRegister cpu a $ readRegister cpu b
runInstruction cpu (Instruction Mov [Address a, Literal 1, Register r]) = do
    writeByteToDevice cpu a $ readRegister cpu r
    return cpu
    
runInstruction cpu (Instruction Mov _) = error "Mov: invalid instruction format."

-- Add
runInstruction cpu (Instruction Add [Register r, Literal l]) = return $ writeRegister cpu r $ readRegister cpu r + l
runInstruction cpu (Instruction Add [Register a, Register b]) = return $ writeRegister cpu a $ readRegister cpu a + readRegister cpu b
runInstruction cpu (Instruction Add _) = error "Add: invalid instruction format."

runInstruction cpu instr = error $ "instruction `" ++ show instr ++ "` is not implemented."

readRegister :: Processor -> Register -> Int
readRegister cpu r = registers cpu !! fromEnum r

writeRegister :: Processor -> Register -> Int -> Processor
writeRegister cpu r v = Processor (updateRegister (registers cpu) (fromEnum r) v) $ devices cpu
    where updateRegister regs i v = take i regs ++ [v] ++ drop (i + 1) regs

deviceAt :: Processor -> Int -> GenericDevice
deviceAt cpu addr = head $ Data.Map.elems $ Data.Map.filterWithKey (\(lo, hi) _ -> addr >= lo && addr < hi) $ devices cpu

writeByteToDevice :: Processor -> Int -> Int -> IO ()
writeByteToDevice cpu addr = writeByte (deviceAt cpu addr) addr
