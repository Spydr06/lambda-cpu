module Instruction
(
    Mnemonic(..),
    Register(..),
    Argument(..),
    Instruction(..),
    Program(..),
    numRegisters
) where

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

numRegisters :: Int
numRegisters = 5
