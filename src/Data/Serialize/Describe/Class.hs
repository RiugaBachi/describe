module Data.Serialize.Describe.Class(
  Describe, describe
) where

import GHC.Generics
import GHC.TypeNats
import Control.Monad
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed (Vec)
import Data.Int
import Data.Proxy
import Data.Word
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Endianness
import qualified Data.Serialize.Describe.Combinators.LE as LE
import qualified Data.Serialize.Describe.Combinators.BE as BE


class Describe a where
  describe :: (s -> a) -> Descriptor s a

  default describe :: (Generic a, GDescribe (Rep a)) => (s -> a) -> Descriptor s a 
  describe f = fmap to . gdescribe $ from <$> f

class GDescribe f where
  gdescribe :: (s -> f a) -> Descriptor s (f a)
  
instance GDescribe U1 where
  gdescribe _ = pure U1

instance (GDescribe a, GDescribe b) => GDescribe (a :*: b) where
  gdescribe f = liftM2 (:*:) (gdescribe (l . f))  (gdescribe (r . f))
    where
      l (a :*: _) = a
      r (_ :*: b) = b

instance (GDescribe a) => GDescribe (M1 i c a) where
  gdescribe f = M1 <$> gdescribe (extract . f)
    where
      extract (M1 x) = x

instance (Describe a) => GDescribe (K1 i a) where
  gdescribe f = K1 <$> describe (extract . f)
    where
      extract (K1 x) = x

instance Describe Bool where
    describe f = toEnum . fromIntegral <$> describe (fromIntegral @_ @Word8 . fromEnum . f)

instance Describe Word8 where
    describe f = Descriptor (unwrapGet (LE.w8 f), \s -> unwrapPut s (LE.w8 f >> pure (f s)))

instance Describe (LE Word16) where
    describe f = Descriptor (unwrapGet (LE <$> LE.w16 (unwrapLE . f)), \s -> unwrapPut s (LE.w16 (unwrapLE . f) >> pure (f s)))

instance Describe (LE Word32) where
    describe f = Descriptor (unwrapGet (LE <$> LE.w32 (unwrapLE . f)), \s -> unwrapPut s (LE.w32 (unwrapLE . f) >> pure (f s)))

instance Describe (LE Word64) where
    describe f = Descriptor (unwrapGet (LE <$> LE.w64 (unwrapLE . f)), \s -> unwrapPut s (LE.w64 (unwrapLE . f) >> pure (f s)))

instance Describe (BE Word16) where
    describe f = Descriptor (unwrapGet (BE <$> BE.w16 (unwrapBE . f)), \s -> unwrapPut s (BE.w16 (unwrapBE . f) >> pure (f s)))

instance Describe (BE Word32) where
    describe f = Descriptor (unwrapGet (BE <$> BE.w32 (unwrapBE . f)), \s -> unwrapPut s (BE.w32 (unwrapBE . f) >> pure (f s)))

instance Describe (BE Word64) where
    describe f = Descriptor (unwrapGet (BE <$> BE.w64 (unwrapBE . f)), \s -> unwrapPut s (BE.w64 (unwrapBE . f) >> pure (f s)))

instance Describe Int8 where
    describe f = Descriptor (unwrapGet (LE.i8 f), \s -> unwrapPut s (LE.i8 f >> pure (f s)))

instance Describe (LE Int16) where
    describe f = Descriptor (unwrapGet (LE <$> LE.i16 (unwrapLE . f)), \s -> unwrapPut s (LE.i16 (unwrapLE . f) >> pure (f s)))

instance Describe (LE Int32) where
    describe f = Descriptor (unwrapGet (LE <$> LE.i32 (unwrapLE . f)), \s -> unwrapPut s (LE.i32 (unwrapLE . f) >> pure (f s)))

instance Describe (LE Int64) where
    describe f = Descriptor (unwrapGet (LE <$> LE.i64 (unwrapLE . f)), \s -> unwrapPut s (LE.i64 (unwrapLE . f) >> pure (f s)))

instance Describe (BE Int16) where
    describe f = Descriptor (unwrapGet (BE <$> BE.i16 (unwrapBE . f)), \s -> unwrapPut s (BE.i16 (unwrapBE . f) >> pure (f s)))

instance Describe (BE Int32) where
    describe f = Descriptor (unwrapGet (BE <$> BE.i32 (unwrapBE . f)), \s -> unwrapPut s (BE.i32 (unwrapBE . f) >> pure (f s)))

instance Describe (BE Int64) where
    describe f = Descriptor (unwrapGet (BE <$> BE.i64 (unwrapBE . f)), \s -> unwrapPut s (BE.i64 (unwrapBE . f) >> pure (f s)))

instance Describe (LE Float) where
    describe f = Descriptor (unwrapGet (LE <$> LE.f32 (unwrapLE . f)), \s -> unwrapPut s (LE.f32 (unwrapLE . f) >> pure (f s)))

instance Describe (LE Double) where
    describe f = Descriptor (unwrapGet (LE <$> LE.f64 (unwrapLE . f)), \s -> unwrapPut s (LE.f64 (unwrapLE . f) >> pure (f s)))

instance Describe (BE Float) where
    describe f = Descriptor (unwrapGet (BE <$> BE.f32 (unwrapBE . f)), \s -> unwrapPut s (BE.f32 (unwrapBE . f) >> pure (f s)))

instance Describe (BE Double) where
    describe f = Descriptor (unwrapGet (BE <$> BE.f64 (unwrapBE . f)), \s -> unwrapPut s (BE.f64 (unwrapBE . f) >> pure (f s)))

instance Describe () where
  describe _ = pure ()

instance (Describe a, V.Arity n, V.Vector (Vec n) a, KnownNat n) => Describe (Vec n a) where
    describe f =
       V.fromList <$> forM [0..fromIntegral (natVal (Proxy :: Proxy n))-1] 
        (\i -> describe $ (V.! i) . f)
