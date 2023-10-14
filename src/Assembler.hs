{-# LANGUAGE OverloadedStrings #-}

module Assembler
(
    assemble    
) where

import Data.Primitive.ByteArray
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word8 (Word8)
import Data.Char
import Control.Arrow (left)
import Text.Printf (printf)
import Data.Text (splitOn, pack, unpack, count)

import Instruction
import Data.Either (partitionEithers)
import Text.Read (readMaybe)
import Numeric (readHex, readBin, readOct)

data Assembler = Assembler {
    filename :: String,
    instructions :: [Instruction],
    labels :: Map String Int,
    relocs :: Map String [Int]
}

data Position = Position {
    filename' :: String,
    line :: String,
    lineno :: Int,
    start :: Int,
    end :: Int
}

instance Show Position where
    show (Position filename line lineno start end) = "\ESC[1m" ++ filename ++ ":" ++ show lineno ++ ":" ++ (if start < 0 then "" else show start ++ ":") ++ " \ESC[0m"

data ParseError = ParseError Position String
instance Show ParseError where
    show (ParseError p e) = show p ++ colorError "parse error: " ++ e ++ "\n" ++ 
                            printf " %*d" (4 :: Int) (lineno p) ++ " | " ++ line p ++ "\n" ++
                            "      | \ESC[34m" ++ (if start p < 0 then 
                                replicate (length $ line p) '~'
                                else replicate (start p) ' ' ++ replicate (end p - start p) '~'
                            ) ++ "\ESC[0m"

newAssembler :: String -> Assembler
newAssembler filename = Assembler filename [] Map.empty Map.empty

assemble :: String -> String -> Either Program ParseError
assemble filename s = left (Program . instructions) $ assembleLines (newAssembler filename) $ zip [1..] $ lines s

assembleLines :: Assembler -> [(Int, String)] -> Either Assembler ParseError
assembleLines a [] = Left a
assembleLines a (x:xs) = case assembleLine a x of
    Left a -> assembleLines a xs
    Right e -> Right e

assembleLine :: Assembler -> (Int, String) -> Either Assembler ParseError
assembleLine a (l, s) | null $ trim code = Left a
                      | otherwise = let (mnemonic, args) = splitAtFirst isSpace code in
                            if last mnemonic == ':' then label a s
                            else parseInstruction a position mnemonic (filter ([] /=) $ map (trim . unpack) $ splitOn "," $ pack args)
                    where code     = ignoreComment s
                          position = Position (filename a) s l (countTrailingSpace s) $ length s

splitAtFirst :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst f = fmap (drop 1) . break f

countTrailingSpace :: String -> Int
countTrailingSpace = count 0
    where count i [] = i
          count i (x:xs) | isSpace x = count (succ i) xs
                         | otherwise = i

parseInstruction :: Assembler -> Position -> String -> [String] -> Either Assembler ParseError
parseInstruction a pos mnStr args = case readMnemonic mnStr of
    Just mnemonic -> case extractError $ map (parseArgument pos) args of
        Left args' -> Left $ appendInstruction a $ Instruction mnemonic args'
        Right err -> Right err 
    Nothing -> Right $ ParseError pos { end = start pos + length mnStr } $ "unknown mnemonic `" ++ mnStr ++ "`."

extractError :: [Either a b] -> Either [a] b
extractError xs = case partitionEithers xs of
    (lefts, []) -> Left lefts
    (_, rights) -> Right $ head rights

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

parseArgument :: Position -> String -> Either Argument ParseError
parseArgument p s | null s = error s
                  | head s == '%' = case readRegister $ tail s of
                      Just r -> Left $ Register r
                      Nothing -> Right $ ParseError p $ "unknown register `" ++ s ++ "`."
                  | head s == '$' = left Literal $ parseNumber p $ tail s
                  | otherwise = case parseNumber p s of
                      Left i -> Left $ Address i
                      Right e -> Right e-- parse label or addr

parseNumber :: Position -> String -> Either Int ParseError
parseNumber p ('0':x:xs) | x == 'x' || x == 'X' = Left $ fst $ head $ readHex xs
                         | x == 'b' || x == 'B' = Left $ fst $ head $ readBin xs
                         | x == 'o' || x == 'O' = Left $ fst $ head $ readOct xs
                         | otherwise = Right $ ParseError p $ "`0" ++ [x] ++ "` is not a valid integer format."
parseNumber p ('\'':x:"'") = Left $ ord x
parseNumber p ('\'':'\\':x:"'") = left ord $ escapeCode p x
parseNumber p c@('\'':_) = Right $ ParseError p $ "Invalid character sequence `" ++ c ++ "`"
parseNumber p s = case readMaybe s of
    Just i -> Left i
    Nothing -> Right $ ParseError p $ "Literal `" ++ s ++ "` is not a valid integer."

escapeCode :: Position -> Char -> Either Char ParseError
escapeCode _ 'a' = Left '\a'
escapeCode _ 'b' = Left '\b'
escapeCode _ 'e' = Left '\ESC'
escapeCode _ 'f' = Left '\f'
escapeCode _ 'n' = Left '\n'
escapeCode _ 'r' = Left '\r'
escapeCode _ 't' = Left '\t'
escapeCode _ 'v' = Left '\v'
escapeCode _ '\\' = Left '\\'
escapeCode _ '\'' = Left '\''
escapeCode _ '"' = Left '"'
escapeCode p c = Right $ ParseError p $ "Invalid escape code `\\" ++ [c] ++ "`."

appendInstruction :: Assembler -> Instruction -> Assembler
appendInstruction a i = a { instructions = instructions a ++ [i] }

label :: Assembler -> String -> Either Assembler ParseError
label a s = undefined

ignoreComment :: String -> String
ignoreComment = unpack . head . splitOn ";" . pack

colorError :: String -> String
colorError s = "\ESC[31m\ESC[1m" ++ s ++ "\ESC[0m" 
