module Data.Serialize.Descriptor.LE(
  w8, w16, w32, w64,
  i8, i16, i32, i64,
  f32, f64
) where

import Data.Word
import Data.Int
import Data.Serialize.IEEE754
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize.Descriptor

w8 :: (a -> Word8) -> Descriptor a Word8
w8 f = Descriptor (getWord8, \s' -> putWord8 (f s'))

w16 :: (a -> Word16) -> Descriptor a Word16
w16 f = Descriptor (getWord16le, \s' -> putWord16le (f s'))

w32 :: (a -> Word32) -> Descriptor a Word32
w32 f = Descriptor (getWord32le, \s' -> putWord32le (f s'))

w64 :: (a -> Word64) -> Descriptor a Word64
w64 f = Descriptor (getWord64le, \s' -> putWord64le (f s'))

i8 :: (a -> Int8) -> Descriptor a Int8
i8 f = Descriptor (getInt8, \s' -> putInt8 (f s'))

i16 :: (a -> Int16) -> Descriptor a Int16
i16 f = Descriptor (getInt16le, \s' -> putInt16le (f s'))

i32 :: (a -> Int32) -> Descriptor a Int32
i32 f = Descriptor (getInt32le, \s' -> putInt32le (f s'))

i64 :: (a -> Int64) -> Descriptor a Int64
i64 f = Descriptor (getInt64le, \s' -> putInt64le (f s'))

f32 :: (a -> Float) -> Descriptor a Float
f32 f = Descriptor (getFloat32le, \s' -> putFloat32le (f s'))

f64 :: (a -> Double) -> Descriptor a Double
f64 f = Descriptor (getFloat64le, \s' -> putFloat64le (f s'))
