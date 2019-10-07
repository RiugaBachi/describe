-- | Big endian combinators.
--
-- All combinators take a function that takes the structure being described ('a') and produces the specified data type from it.
-- Most of the time, this will be one of the structure's fields, which are all functions from the structure to the field type.

module Data.Serialize.Describe.Combinators.BE(
  w8, w16, w32, w64,
  i8, i16, i32, i64,
  f32, f64
) where

import Data.Word
import Data.Int
import Data.Serialize.IEEE754
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize.Describe.Descriptor

w8 :: (a -> Word8) -> Descriptor a Word8
w8 f = Descriptor (getWord8, \s' -> putWord8 (f s') >> pure (f s'))

w16 :: (a -> Word16) -> Descriptor a Word16
w16 f = Descriptor (getWord16be, \s' -> putWord16be (f s') >> pure (f s'))

w32 :: (a -> Word32) -> Descriptor a Word32
w32 f = Descriptor (getWord32be, \s' -> putWord32be (f s') >> pure (f s'))

w64 :: (a -> Word64) -> Descriptor a Word64
w64 f = Descriptor (getWord64be, \s' -> putWord64be (f s') >> pure (f s'))

i8 :: (a -> Int8) -> Descriptor a Int8
i8 f = Descriptor (getInt8, \s' -> putInt8 (f s') >> pure (f s'))

i16 :: (a -> Int16) -> Descriptor a Int16
i16 f = Descriptor (getInt16be, \s' -> putInt16be (f s') >> pure (f s'))

i32 :: (a -> Int32) -> Descriptor a Int32
i32 f = Descriptor (getInt32be, \s' -> putInt32be (f s') >> pure (f s'))

i64 :: (a -> Int64) -> Descriptor a Int64
i64 f = Descriptor (getInt64be, \s' -> putInt64be (f s') >> pure (f s'))

f32 :: (a -> Float) -> Descriptor a Float
f32 f = Descriptor (getFloat32be, \s' -> putFloat32be (f s') >> pure (f s'))

f64 :: (a -> Double) -> Descriptor a Double
f64 f = Descriptor (getFloat64be, \s' -> putFloat64be (f s') >> pure (f s'))