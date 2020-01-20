module Data.Serialize.Describe.Combinators.Const where

import GHC.TypeNats
import Data.Serialize.Describe
import Data.Proxy

-- | A type level wrapper around the equivalent of the @pure . const@ descriptor forall @KnownNat@.
data Const (n :: Nat) t = Const

instance KnownNat n => Show (Const n t) where
  show _ = show $ natVal (Proxy :: Proxy n)

instance (KnownNat n, Describe t, Integral t) => Describe (Const n t) where
  type Context m (Const n t) = Context m t
  describe = (Const <$) . field @t $ const $ fromIntegral $ natVal $ (Proxy :: Proxy n)

