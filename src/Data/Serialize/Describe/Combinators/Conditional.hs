module Data.Serialize.Describe.Combinators.Conditional where

import Prelude hiding (id, (.))
import GHC.TypeNats
import Data.Proxy
import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed
import Data.Serialize.Describe.Internal.Descriptor
import Data.Serialize.Describe.Class
import Control.Monad.Trans.Control

-- | An 'Optional' represents a field which is optionally-serializable. The field will be parsed via a lookAhead and, if the value matches the 'Predicate' p, then the field exists. If not, it is assumed as though the field was never serialized in the first place and the value will be set to 'Nothing'; parsing will then continue on as usual.
newtype Optional p t = Optional { unwrapOptional :: Maybe t }

class Predicate t a where
  check :: t -> Bool

data Equals (n :: Nat)

instance (KnownNat n, Integral i) => Predicate i (Equals n) where
  check i = i == (fromIntegral $ natVal (Proxy :: Proxy n))

instance {-# OVERLAPPING #-} (KnownNat n1, KnownNat n2, V.Arity n2, V.Vector (Vec n2) i, Integral i) => Predicate (Vec n2 i) (Equals n1) where
  check = V.all (== fromIntegral (natVal (Proxy :: Proxy n1)))

instance (Describe a, Predicate a p) => Describe (Optional p a) where
  type Context m (Optional p a) = (MonadTransControl m, Context m a)
  describe = Optional <$> conditionally unwrapOptional (check @a @p) describe
