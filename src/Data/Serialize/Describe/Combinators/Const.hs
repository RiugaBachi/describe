module Data.Serialize.Describe.Combinators.Const where

import GHC.TypeNats
import Data.Serialize.Describe
import Data.Proxy

-- | A type level wrapper around the equivalent of the @pure . const@ descriptor.
data Const (n :: Nat) t = Const

instance KnownNat n => Show (Const n t) where
  show _ = show $ natVal (Proxy :: Proxy n)

instance (KnownNat n, Describe t, Integral t) => Describe (Const n t) where
  describe _ = (Const <$) . describe @t $ const $ fromIntegral $ natVal $ (Proxy :: Proxy n)

