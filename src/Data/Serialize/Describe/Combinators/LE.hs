-- | Little endian combinators.
--
-- All combinators take a function that takes the structure leing described ('a') and produces the specified data type from it.
-- Most of the time, this will le one of the structure's fields, which are all functions from the structure to the field type.
module Data.Serialize.Describe.Combinators.LE(
  LE(..),
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
w16 = isoField @(LE Word16) fi

w32 :: Integral i => (s -> i) -> Descriptor s i
w32 = isoField @(LE Word32) fi

w64 :: Integral i => (s -> i) -> Descriptor s i
w64 = isoField @(LE Word64) fi

i16 :: Integral i => (s -> i) -> Descriptor s i
i16 = isoField @(LE Int16) fi

i32 :: Integral i => (s -> i) -> Descriptor s i
i32 = isoField @(LE Int32) fi

i64 :: Integral i => (s -> i) -> Descriptor s i
i64 = isoField @(LE Int64) fi

f32 :: (Real f, Fractional f) => (s -> f) -> Descriptor s f
f32 = isoField @(LE Float) rtf

f64 :: (Real f, Fractional f) => (s -> f) -> Descriptor s f
f64 = isoField @(LE Double) rtf

newtype LE a 
  = LE { unwrapLE :: a }
  deriving newtype (Show, Read, Num, Eq, Ord, Enum, Integral, Real, Fractional)

type role LE representational

instance Describe (LE Word16) where
    describe = mkDescriptor fi (const 2) getWord16le putWord16le

instance Describe (LE Word32) where
    describe = mkDescriptor fi (const 4) getWord32le putWord32le

instance Describe (LE Word64) where
    describe = mkDescriptor fi (const 8) getWord64le putWord64le

instance Describe (LE Int16) where
    describe = mkDescriptor fi (const 2) getInt16le putInt16le

instance Describe (LE Int32) where
    describe = mkDescriptor fi (const 4) getInt32le putInt32le

instance Describe (LE Int64) where
    describe = mkDescriptor fi (const 8) getInt64le putInt64le

instance Describe (LE Float) where
    describe = mkDescriptor rtf (const 4) getFloat32le putFloat32le

instance Describe (LE Double) where
    describe = mkDescriptor rtf (const 8) getFloat64le putFloat64le
