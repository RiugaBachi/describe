-- | Big endian combinators.
--
-- All combinators take a function that takes the structure being described ('a') and produces the specified data type from it.
-- Most of the time, this will be one of the structure's fields, which are all functions from the structure to the field type.

module Data.Serialize.Describe.Combinators.BE(
  BE(..),
  w16, w32, w64,
  i16, i32, i64,
  f32, f64
) where

import Data.Word
import Data.Int
import Data.Serialize.IEEE754
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Class

w16 :: Integral i => (s -> i) -> Descriptor s i
w16 f = Descriptor (fromIntegral <$> getWord16be, \s' -> putWord16be (fromIntegral $ f s') >> pure (f s'))

w32 :: Integral i => (s -> i) -> Descriptor s i
w32 f = Descriptor (fromIntegral <$> getWord32be, \s' -> putWord32be (fromIntegral $ f s') >> pure (f s'))

w64 :: Integral i => (s -> i) -> Descriptor s i
w64 f = Descriptor (fromIntegral <$> getWord64be, \s' -> putWord64be (fromIntegral $ f s') >> pure (f s'))

i16 :: Integral i => (s -> i) -> Descriptor s i
i16 f = Descriptor (fromIntegral <$> getInt16be, \s' -> putInt16be (fromIntegral $ f s') >> pure (f s'))

i32 :: Integral i => (s -> i) -> Descriptor s i
i32 f = Descriptor (fromIntegral <$> getInt32be, \s' -> putInt32be (fromIntegral $ f s') >> pure (f s'))

i64 :: Integral i => (s -> i) -> Descriptor s i
i64 f = Descriptor (fromIntegral <$> getInt64be, \s' -> putInt64be (fromIntegral $ f s') >> pure (f s'))

f32 :: (Real f, Fractional f) => (s -> f) -> Descriptor s f
f32 f = Descriptor (realToFrac <$> getFloat32be, \s' -> putFloat32be (realToFrac $ f s') >> pure (f s'))

f64 :: (Real f, Fractional f) => (s -> f) -> Descriptor s f
f64 f = Descriptor (realToFrac <$> getFloat64be, \s' -> putFloat64be (realToFrac $ f s') >> pure (f s'))

newtype BE a = BE { unwrapBE :: a }
             deriving (Show, Read, Num, Eq, Ord, Enum, Integral, Real, Fractional)

instance Describe (BE Word16) where
    describe = w16

instance Describe (BE Word32) where
    describe = w32

instance Describe (BE Word64) where
    describe = w64

instance Describe (BE Int16) where
    describe = i16

instance Describe (BE Int32) where
    describe = i32

instance Describe (BE Int64) where
    describe = i64

instance Describe (BE Float) where
    describe = f32

instance Describe (BE Double) where
    describe = f64

