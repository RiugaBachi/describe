module Data.Serialize.Describe.Class.Instances where

import GHC.TypeNats
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed (Vec)
import Data.Int
import Data.Word
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Combinators
import qualified Data.Serialize.Describe.Combinators.LE as LE
import qualified Data.Serialize.Describe.Combinators.BE as BE

instance Describe Bool where
    describe f = toEnum . fromIntegral <$> describe (fromIntegral @_ @Word8 . fromEnum . f)

instance Describe Word8 where
    describe f = Descriptor (unwrapGet (LE.w8 f), \s -> unwrapPut s (LE.w8 f >> pure (f s)))

instance Describe (LE Word16) where
    describe f = Descriptor (unwrapGet (LE.w16 f), \s -> unwrapPut s (LE.w16 f >> pure (f s)))

instance Describe (LE Word32) where
    describe f = Descriptor (unwrapGet (LE.w32 f), \s -> unwrapPut s (LE.w32 f >> pure (f s)))

instance Describe (LE Word64) where
    describe f = Descriptor (unwrapGet (LE.w64 f), \s -> unwrapPut s (LE.w64 f >> pure (f s)))

instance Describe (BE Word16) where
    describe f = Descriptor (unwrapGet (BE.w16 f), \s -> unwrapPut s (BE.w16 f >> pure (f s)))

instance Describe (BE Word32) where
    describe f = Descriptor (unwrapGet (BE.w32 f), \s -> unwrapPut s (BE.w32 f >> pure (f s)))

instance Describe (BE Word64) where
    describe f = Descriptor (unwrapGet (BE.w64 f), \s -> unwrapPut s (BE.w64 f >> pure (f s)))

instance Describe Int8 where
    describe f = Descriptor (unwrapGet (LE.i8 f), \s -> unwrapPut s (LE.i8 f >> pure (f s)))

instance Describe (LE Int16) where
    describe f = Descriptor (unwrapGet (LE.i16 f), \s -> unwrapPut s (LE.i16 f >> pure (f s)))

instance Describe (LE Int32) where
    describe f = Descriptor (unwrapGet (LE.i32 f), \s -> unwrapPut s (LE.i32 f >> pure (f s)))

instance Describe (LE Int64) where
    describe f = Descriptor (unwrapGet (LE.i64 f), \s -> unwrapPut s (LE.i64 f >> pure (f s)))

instance Describe (BE Int16) where
    describe f = Descriptor (unwrapGet (BE.i16 f), \s -> unwrapPut s (BE.i16 f >> pure (f s)))

instance Describe (BE Int32) where
    describe f = Descriptor (unwrapGet (BE.i32 f), \s -> unwrapPut s (BE.i32 f >> pure (f s)))

instance Describe (BE Int64) where
    describe f = Descriptor (unwrapGet (BE.i64 f), \s -> unwrapPut s (BE.i64 f >> pure (f s)))

instance Describe (LE Float) where
    describe f = Descriptor (unwrapGet (LE.f32 f), \s -> unwrapPut s (LE.f32 f >> pure (f s)))

instance Describe (LE Double) where
    describe f = Descriptor (unwrapGet (LE.f64 f), \s -> unwrapPut s (LE.f64 f >> pure (f s)))

instance Describe (BE Float) where
    describe f = Descriptor (unwrapGet (BE.f32 f), \s -> unwrapPut s (BE.f32 f >> pure (f s)))

instance Describe (BE Double) where
    describe f = Descriptor (unwrapGet (BE.f64 f), \s -> unwrapPut s (BE.f64 f >> pure (f s)))

instance Describe () where
  describe f = pure ()

instance (Describe a, V.Vector (Vec n) a, KnownNat n) => Describe (Vec n a) where
    describe f =
       V.fromList <$> forM [0..fromIntegral (natVal (Proxy :: Proxy n))-1] 
        (\i -> describe $ (V.! i) . f)
