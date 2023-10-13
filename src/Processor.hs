module Processor
(
    Processor(..),
    defaultFlags,
    runProgram
) where

import Data.Map (Map)
import qualified Data.Map
import Data.Maybe (listToMaybe)
import Numeric (showHex)

import Instruction
import Device

data Flags = Flags {
    halt :: Bool,
    zero :: Bool,
    less :: Bool,
    greater :: Bool,
    carry :: Bool
}

defaultFlags :: Flags
defaultFlags = Flags False False False False False

data Processor = Processor {
    registers :: [Int],
    flags :: Flags,
    devices :: Map (Int, Int) GenericDevice
}

instance Show Processor where
    show = foldl (++) "registers:\n" . zipWith showRegister [0..] . registers
        where showRegister i r = show (toEnum i :: Register) ++ ":\t" ++ show r ++ "\n"

-- like `!!` but returning `Maybe`
infix 9 !?
(!?) :: [a] -> Int -> Maybe a
xs !? i | i < 0     = Nothing
        | otherwise = listToMaybe (drop i xs)

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
runInstruction cpu (Instruction Jmp [Literal l]) = return $ jmp cpu l True
runInstruction cpu (Instruction Jmp [Register r]) = return $ jmp cpu (readRegister cpu r) True

-- Jz
runInstruction cpu (Instruction Jz [Literal l]) = return $ jmp cpu l $ zero $ flags cpu
runInstruction cpu (Instruction Jz [Register r]) = return $ jmp cpu (readRegister cpu r) $ zero $ flags cpu

-- Jnz
runInstruction cpu (Instruction Jnz [Literal l]) = return $ jmp cpu l $ not $ zero $ flags cpu
runInstruction cpu (Instruction Jnz [Register r]) = return $ jmp cpu (readRegister cpu r) $ not $ zero $ flags cpu

-- Jg
runInstruction cpu (Instruction Jg [Literal l]) = return $ jmp cpu l $ greater $ flags cpu
runInstruction cpu (Instruction Jg [Register r]) = return $ jmp cpu (readRegister cpu r) $ greater $ flags cpu

-- Jgz
runInstruction cpu (Instruction Jgz [Literal l]) = return $ jmp cpu l $ (greater . flags) cpu || (zero . flags) cpu
runInstruction cpu (Instruction Jgz [Register r]) = return $ jmp cpu (readRegister cpu r) $ (greater . flags) cpu || (zero . flags) cpu

-- Jl
runInstruction cpu (Instruction Jl [Literal l]) = return $ jmp cpu l $ less $ flags cpu
runInstruction cpu (Instruction Jl [Register r]) = return $ jmp cpu (readRegister cpu r) $ less $ flags cpu

-- Jlz
runInstruction cpu (Instruction Jlz [Literal l]) = return $ jmp cpu l $ (less . flags) cpu || (zero . flags) cpu
runInstruction cpu (Instruction Jlz [Register r]) = return $ jmp cpu (readRegister cpu r) $ (less . flags) cpu || (zero . flags) cpu

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
runInstruction cpu (Instruction Add [Register a, Register b]) = return $ binop cpu a (readRegister cpu b) (+)
runInstruction cpu (Instruction Add [Register r, Literal l]) = return $ binop cpu r l (+)

-- Sub
runInstruction cpu (Instruction Sub [Register a, Register b]) = return $ binop cpu a (readRegister cpu b) (-)
runInstruction cpu (Instruction Sub [Register r, Literal l]) = return $ binop cpu r l (-)

-- Mul
runInstruction cpu (Instruction Mul [Register a, Register b]) = return $ binop cpu a (readRegister cpu b) (*)
runInstruction cpu (Instruction Mul [Register r, Literal l]) = return $ binop cpu r l (*)

-- Div
runInstruction cpu (Instruction Div [Register a, Register b]) = return $ binop cpu a (readRegister cpu b) div
runInstruction cpu (Instruction Div [Register r, Literal l]) = return $ binop cpu r l div

-- Cl
runInstruction cpu (Instruction Cl []) = return $ clearFlags cpu

-- default case
runInstruction cpu (Instruction mnemonic _) = error $ show mnemonic ++ ": invalid instruction format."

-- CPU comparisons

cmp :: Processor -> Int -> Int -> Processor
cmp cpu a b = setFlags cpu flags' { zero = a == b, less = a < b, greater = a > b }
    where flags' = flags cpu

jmp :: Processor -> Int -> Bool -> Processor
jmp cpu addr doJmp = if doJmp then writeIP cpu $ pred addr else cpu

binop :: Processor -> Register -> Int -> (Int -> Int -> Int) -> Processor
binop cpu a b op = writeRegister cpu a $ readRegister cpu a `op` b

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
