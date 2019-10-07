module Data.Serialize.Describe.Combinators(
  Optional(..),
  StaticPred(..),
  LE(..),
  BE(..)
) where

import GHC.TypeNats
import Data.Proxy
import Data.Maybe
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Class
import qualified Data.Serialize.Get as G

newtype LE a = LE { unwrapLE :: a }

newtype BE a = BE { unwrapBE :: a }

newtype Optional p t = Optional { unwrapOptional :: Maybe t }

class StaticPred t a where
  check :: t -> Bool

data Equals (n :: Nat)

instance (KnownNat n, Integral i) => StaticPred i (Equals n) where
  check i = i == (fromIntegral $ natVal (Proxy :: Proxy n))

instance (Describe a, StaticPred a p) => Describe (Optional p a) where
  describe f = Descriptor (g, p)
    where
      g = do 
        let d = unwrapGet $ describe @a $ fromJust . unwrapOptional . f
        v <- G.lookAhead d
        Optional <$> if check @a @p v then Just <$> d else pure Nothing
      p s = case unwrapOptional $ f s of
        Just x -> Optional . Just <$> unwrapPut s (describe $ const x)
        Nothing -> pure $ Optional Nothing
