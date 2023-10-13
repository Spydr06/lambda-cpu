{-# LANGUAGE ExistentialQuantification #-}

module Device 
(
    GenericDevice(..),
    StdIO(..),
    ReadOnlyMemory(..),

    writeByte,
    readByte
) where

import Data.Primitive.ByteArray
import Data.Char (ord, chr)

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
