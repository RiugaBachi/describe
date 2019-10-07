module Data.Serialize.Describe.Class(
  Describe, describe
) where

import GHC.Generics
import Control.Monad
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

