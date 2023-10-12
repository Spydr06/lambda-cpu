{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Char (ord, chr)
import Data.Primitive.ByteArray
import Data.List.Extra ((!?))
import Data.Map (Map)
import qualified Data.Map
import Numeric (showHex)

data Mnemonic = Nop
    | Hlt
    | Jmp
    | Mov
    | Add
    | Sub
    deriving (Show, Eq)

data Register = X0
    | X1
    | X2
    | X3
    | IP
    deriving (Show, Enum)

data Argument = Register Register
    | Address Int
    | Literal Int
    deriving Show

data Instruction = Instruction Mnemonic [Argument]
    deriving Show

newtype Program = Program [Instruction] 

newtype Flags = Flags {
    halt :: Bool
}

data Processor = Processor {
    registers :: [Int],
    flags :: Flags,
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

newtype ReadOnlyMemory = ReadOnlyMemory ByteArray
instance Device ReadOnlyMemory where
    readByte (ReadOnlyMemory bytes) addr 
        | addr < sizeofByteArray bytes = return $ indexByteArray bytes addr
        | otherwise                    = return 0x00
    writeByte _ addr _ = error $ "tried writing to read-only memory at " ++ show addr

numRegisters :: Int
numRegisters = 5

defaultFlags :: Flags
defaultFlags = Flags False

newProcessor :: Processor
newProcessor = Processor (replicate numRegisters 0) defaultFlags $ Data.Map.fromList [
        ((0x0000, 0xf000), GenericDevice $ ReadOnlyMemory $ byteArrayFromList [34 :: Int]),
        ((0xfffe, 0xffff), GenericDevice $ StdIO ())
    ]

main :: IO ()
main = do
    result <- runProgram newProcessor $ Program [
            Instruction Nop [],
            Instruction Mov [Register X0, Literal 1, Address 0x0000],
            Instruction Mov [Register X1, Literal 35],
            Instruction Add [Register X0, Register X1],
            Instruction Mov [Address 0xfffe, Literal 1, Register X0],
            Instruction Jmp [Literal 0],
            Instruction Hlt []
        ]
    putChar '\n'
    print result

runProgram :: Processor -> Program -> IO Processor
runProgram cpu (Program instr) = do
    cpu' <- runInstruction cpu $ case instr !? readIP cpu of
        Just a -> a        
        Nothing -> error $ "No instruction at address 0x" ++ showHex (readIP cpu) ""
    if halted cpu' then return cpu'
    else runProgram (incrementIP cpu') $ Program instr

runInstruction :: Processor -> Instruction -> IO Processor

-- Nop
runInstruction cpu (Instruction Nop []) = return cpu
runInstruction cpu (Instruction Nop _) = error "Nop: invalid instruction format."

-- Hlt
runInstruction cpu (Instruction Hlt []) = return $ haltCpu cpu
runInstruction cpu (Instruction Hlt _) = error "Hlt: invalid instruciton format."

-- Jmp
runInstruction cpu (Instruction Jmp [Literal l]) = return $ writeIP cpu $ pred l
runInstruction cpu (Instruction Jmp [Register r]) = return $ writeIP cpu $ pred $ readRegister cpu r
runInstruction cpu (Instruction Jmp _) = error "Jmp: invalid instruciton format."

-- Mov
runInstruction cpu (Instruction Mov [Register r, Literal l]) = return $ writeRegister cpu r l
runInstruction cpu (Instruction Mov [Register a, Register b]) = return $ writeRegister cpu a $ readRegister cpu b
runInstruction cpu (Instruction Mov [Address a, Literal 1, Register r]) = do
    writeByteToDevice cpu a $ readRegister cpu r
    return cpu
runInstruction cpu (Instruction Mov [Register r, Literal 1, Address a]) = do
    byte <- readByteFromDevice cpu a
    return $ writeRegister cpu r byte
runInstruction cpu (Instruction Mov _) = error "Mov: invalid instruction format."

-- Add
runInstruction cpu (Instruction Add [Register r, Literal l]) = return $ writeRegister cpu r $ readRegister cpu r + l
runInstruction cpu (Instruction Add [Register a, Register b]) = return $ writeRegister cpu a $ readRegister cpu a + readRegister cpu b
runInstruction cpu (Instruction Add _) = error "Add: invalid instruction format."

runInstruction cpu instr = error $ "instruction `" ++ show instr ++ "` is not implemented."

-- CPU registers:

readRegister :: Processor -> Register -> Int
readRegister cpu r = registers cpu !! fromEnum r

writeRegister :: Processor -> Register -> Int -> Processor
writeRegister cpu r v = Processor (updateRegister (registers cpu) (fromEnum r) v) (flags cpu) $ devices cpu
    where updateRegister regs i v = take i regs ++ [v] ++ drop (i + 1) regs

readIP :: Processor -> Int
readIP cpu = readRegister cpu IP

writeIP :: Processor -> Int -> Processor
writeIP cpu = writeRegister cpu IP

incrementIP :: Processor -> Processor
incrementIP = writeIP <*> succ . readIP

-- CPU flags:

halted :: Processor -> Bool
halted = halt . flags

setHalted :: Flags -> Flags
setHalted _ = Flags True

haltCpu :: Processor -> Processor
haltCpu = Processor <$> registers <*> setHalted . flags <*> devices

-- general device IO:

deviceAt :: Processor -> Int -> GenericDevice
deviceAt cpu addr = head $ Data.Map.elems $ Data.Map.filterWithKey (\(lo, hi) _ -> addr >= lo && addr < hi) $ devices cpu

writeByteToDevice :: Processor -> Int -> Int -> IO ()
writeByteToDevice cpu addr = writeByte (deviceAt cpu addr) addr

readByteFromDevice :: Processor -> Int -> IO Int
readByteFromDevice cpu addr = readByte (deviceAt cpu addr) addr

