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
import Data.Serialize.Describe.Internal.Descriptor
import Data.Serialize.Describe.Isomorphisms
import Data.Serialize.Describe.Class

w16 :: Integral i => (s -> i) -> Descriptor s i
w16 = isoField @(BE Word16) fi

w32 :: Integral i => (s -> i) -> Descriptor s i
w32 = isoField @(BE Word32) fi

w64 :: Integral i => (s -> i) -> Descriptor s i
w64 = isoField @(BE Word64) fi

i16 :: Integral i => (s -> i) -> Descriptor s i
i16 = isoField @(BE Int16) fi

i32 :: Integral i => (s -> i) -> Descriptor s i
i32 = isoField @(BE Int32) fi

i64 :: Integral i => (s -> i) -> Descriptor s i
i64 = isoField @(BE Int64) fi

f32 :: (Real f, Fractional f) => (s -> f) -> Descriptor s f
f32 = isoField @(BE Float) rtf

f64 :: (Real f, Fractional f) => (s -> f) -> Descriptor s f
f64 = isoField @(BE Double) rtf

newtype BE a
  = BE { unwrapBE :: a }
  deriving newtype (Show, Read, Num, Eq, Ord, Enum, Integral, Real, Fractional)

type role BE representational

instance Describe (BE Word16) where
    describe = mkDescriptor fi (const 2) getWord16be putWord16be

instance Describe (BE Word32) where
    describe = mkDescriptor fi (const 4) getWord32be putWord32be

instance Describe (BE Word64) where
    describe = mkDescriptor fi (const 8) getWord64be putWord64be

instance Describe (BE Int16) where
    describe = mkDescriptor fi (const 2) getInt16be putInt16be

instance Describe (BE Int32) where
    describe = mkDescriptor fi (const 4) getInt32be putInt32be

instance Describe (BE Int64) where
    describe = mkDescriptor fi (const 8) getInt64be putInt64be

instance Describe (BE Float) where
    describe = mkDescriptor rtf (const 4) getFloat32be putFloat32be

instance Describe (BE Double) where
    describe = mkDescriptor rtf (const 8) getFloat64be putFloat64be
