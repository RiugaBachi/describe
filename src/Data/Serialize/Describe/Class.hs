module Data.Serialize.Describe.Class(
  Describe, describe
) where

import GHC.Generics
import GHC.TypeNats
import Control.Monad
import qualified Data.Vector.Fixed as V
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Vector.Fixed.Boxed (Vec)
import Data.Int
import Data.Proxy
import Data.Word
import Data.Serialize.Describe.Descriptor

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

instance Describe () where
  describe _ = pure ()

instance Describe Bool where
    describe f = toEnum . fromIntegral <$> describe (fromIntegral @_ @Word8 . fromEnum . f)

instance Describe Word8 where
    describe f = Descriptor (fromIntegral <$> getWord8, \s' -> putWord8 (fromIntegral $ f s') >> pure (fromIntegral $ f s'))

instance Describe Int8 where
    describe f = Descriptor (fromIntegral <$> getInt8, \s' -> putInt8 (fromIntegral $ f s') >> pure (f s'))

instance (Describe a, V.Arity n, V.Vector (Vec n) a, KnownNat n) => Describe (Vec n a) where
    describe f =
       V.fromList <$> forM [0..fromIntegral (natVal (Proxy :: Proxy n))-1] 
        (\i -> describe $ (V.! i) . f)
