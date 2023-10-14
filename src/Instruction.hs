module Instruction
(
    Mnemonic(..),
    Register(..),
    Argument(..),
    Instruction(..),
    Program(..),
    numRegisters,
    readMnemonic,
    readRegister
) where

import Data.Char (toUpper)

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
    | Sub
    | Mul
    | Div
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
    deriving Show
 
numRegisters :: Int
numRegisters = 5

readMnemonic :: String -> Maybe Mnemonic 
readMnemonic s = case map toUpper s of
    "NOP" -> Just Nop
    "HLT" -> Just Hlt
    "CMP" -> Just Cmp
    "JZ" -> Just Jz
    "JNZ" -> Just Jnz
    "JG" -> Just Jg
    "JGZ" -> Just Jgz
    "JL" -> Just Jl
    "JLZ" -> Just Jlz
    "MOV" -> Just Mov
    "ADD" -> Just Add
    "SUB" -> Just Sub
    "MUL" -> Just Mul
    "DIV" -> Just Div
    "CL" -> Just Cl
    _ -> Nothing

readRegister :: String -> Maybe Register
readRegister s = case map toUpper s of
    "X0" -> Just X0
    "X1" -> Just X1
    "X2" -> Just X2
    "X3" -> Just X3
    "IP" -> Just IP
    _ -> Nothing

