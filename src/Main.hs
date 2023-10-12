{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Char (ord, chr)
import Data.Primitive.ByteArray
import Data.Map (Map)
import qualified Data.Map
import Numeric (showHex)
import Data.Maybe (listToMaybe)

data Mnemonic = Nop
    | Hlt
    | Cmp
    | Jmp
    | Jz
    | Jnz
    | Jg
    | Jgz
    | Jl
    | Jlz
    | Mov
    | Add
    | Cl
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

data Flags = Flags {
    halt :: Bool,
    zero :: Bool,
    less :: Bool,
    greater :: Bool
}

data Processor = Processor {
    registers :: [Int],
    flags :: Flags,
    devices :: Map (Int, Int) GenericDevice
}

instance Show Processor where
    show = foldl (++) "registers:\n" . zipWith showRegister [0..] . registers
        where showRegister i r = show (toEnum i :: Register) ++ ":\t" ++ show r ++ "\n"

class Device a where
    readByte :: a -> Int -> IO Int
    writeByte :: a -> Int -> Int -> IO ()

data GenericDevice = forall a. Device a => GenericDevice {
    device :: a,
    base :: Int
}

instance Device GenericDevice where
    readByte (GenericDevice a base) addr = readByte a $ addr - base
    writeByte (GenericDevice a base) addr = writeByte a $ addr - base

data StdIO = StdIO
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
defaultFlags = Flags False False False False

newProcessor :: Processor
newProcessor = Processor (replicate numRegisters 0) defaultFlags $ Data.Map.fromList [
        device 0x0000 0xf000 $ ReadOnlyMemory $ byteArrayFromList [34 :: Int],
        device 0xfffe 0xffff StdIO
    ]
    where device lo hi d = ((lo, hi), GenericDevice d lo)

-- like `!!` but returning `Maybe`
infix 9 !?
(!?) :: [a] -> Int -> Maybe a
xs !? i | i < 0     = Nothing
        | otherwise = listToMaybe (drop i xs)

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

-- Hlt
runInstruction cpu (Instruction Hlt []) = return $ haltCpu cpu

-- Cmp
runInstruction cpu (Instruction Cmp [Register a, Register b]) = return $ cmp cpu (readRegister cpu a) $ readRegister cpu b
runInstruction cpu (Instruction Cmp [Register r, Literal l]) = return $ cmp cpu (readRegister cpu r) l

-- Jmp
runInstruction cpu (Instruction Jmp [Literal l]) = return $ writeIP cpu $ pred l
runInstruction cpu (Instruction Jmp [Register r]) = return $ writeIP cpu $ pred $ readRegister cpu r

-- Jz
runInstruction cpu (Instruction Jz [Literal l]) = return $ if zero $ flags cpu then writeIP cpu $ pred l else cpu
runInstruction cpu (Instruction Jz [Register r]) = return $ if zero $ flags cpu then writeIP cpu $ pred $ readRegister cpu r else cpu

-- Jnz
runInstruction cpu (Instruction Jnz [Literal l]) = return $ if not $ zero $ flags cpu then writeIP cpu $ pred l else cpu
runInstruction cpu (Instruction Jnz [Register r]) = return $ if not $ zero $ flags cpu then writeIP cpu $ pred $ readRegister cpu r else cpu

-- Jg
runInstruction cpu (Instruction Jg [Literal l]) = return $ if greater $ flags cpu then writeIP cpu $ pred l else cpu
runInstruction cpu (Instruction Jg [Register r]) = return $ if greater $ flags cpu then writeIP cpu $ pred $ readRegister cpu r else cpu

-- Jgz
runInstruction cpu (Instruction Jgz [Literal l]) = return $ if (greater . flags) cpu || (zero . flags) cpu then writeIP cpu $ pred l else cpu
runInstruction cpu (Instruction Jgz [Register r]) = return $ if (greater . flags) cpu || (zero . flags) cpu then writeIP cpu $ pred $ readRegister cpu r else cpu

-- Jl
runInstruction cpu (Instruction Jl [Literal l]) = return $ if less $ flags cpu then writeIP cpu $ pred l else cpu
runInstruction cpu (Instruction Jl [Register r]) = return $ if less $ flags cpu then writeIP cpu $ pred $ readRegister cpu r else cpu

-- Jlz
runInstruction cpu (Instruction Jlz [Literal l]) = return $ if (less . flags) cpu || (zero . flags) cpu then writeIP cpu $ pred l else cpu
runInstruction cpu (Instruction Jlz [Register r]) = return $ if (less . flags) cpu || (zero . flags) cpu then writeIP cpu $ pred $ readRegister cpu r else cpu

-- Mov
runInstruction cpu (Instruction Mov [Register r, Literal l]) = return $ writeRegister cpu r l
runInstruction cpu (Instruction Mov [Register a, Register b]) = return $ writeRegister cpu a $ readRegister cpu b
runInstruction cpu (Instruction Mov [Address a, Literal 1, Register r]) = do
    writeByteToDevice cpu a $ readRegister cpu r
    return cpu
runInstruction cpu (Instruction Mov [Register r, Literal 1, Address a]) = do
    byte <- readByteFromDevice cpu a
    return $ writeRegister cpu r byte

-- Add
runInstruction cpu (Instruction Add [Register r, Literal l]) = return $ writeRegister cpu r $ readRegister cpu r + l
runInstruction cpu (Instruction Add [Register a, Register b]) = return $ writeRegister cpu a $ readRegister cpu a + readRegister cpu b

-- Cl
runInstruction cpu (Instruction Cl []) = return $ clearFlags cpu

-- default case
runInstruction cpu (Instruction mnemonic _) = error $ show mnemonic ++ ": invalid instruction format."

-- CPU comparisons

cmp :: Processor -> Int -> Int -> Processor
cmp cpu a b = setFlags cpu flags' { zero = a == b, less = a < b, greater = a > b }
    where flags' = flags cpu

-- CPU registers:

readRegister :: Processor -> Register -> Int
readRegister cpu r = registers cpu !! fromEnum r

writeRegister :: Processor -> Register -> Int -> Processor
writeRegister cpu r v = Processor (updateRegister (registers cpu) (fromEnum r) v) (flags cpu) $ devices cpu
    where updateRegister regs i v = take i regs ++ [v] ++ drop (succ i) regs

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
setHalted flags = flags { halt = True }

haltCpu :: Processor -> Processor
haltCpu = Processor <$> registers <*> setHalted . flags <*> devices

setFlags :: Processor -> Flags -> Processor
setFlags cpu flags = cpu { flags = flags }

clearFlags :: Processor -> Processor
clearFlags cpu = cpu { flags = defaultFlags }

-- general device IO:

deviceAt :: Processor -> Int -> GenericDevice
deviceAt cpu addr = head $ Data.Map.elems $ Data.Map.filterWithKey (\(lo, hi) _ -> addr >= lo && addr < hi) $ devices cpu

writeByteToDevice :: Processor -> Int -> Int -> IO ()
writeByteToDevice cpu addr = writeByte (deviceAt cpu addr) addr

readByteFromDevice :: Processor -> Int -> IO Int
readByteFromDevice cpu addr = readByte (deviceAt cpu addr) addr

