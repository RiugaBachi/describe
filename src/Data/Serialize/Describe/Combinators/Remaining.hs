module Data.Serialize.Describe.Combinators.Remaining where

import Data.ByteString
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Class

-- | A  'Remaining' represents the rest of the buffer. Upon serialization, the entire wrapped ByteString will be written.
newtype Remaining = Remaining { unwrapRemaining :: ByteString }

instance Describe Remaining where
  describe f = Descriptor (fmap Remaining . G.getByteString =<< G.remaining, \s -> P.putByteString (unwrapRemaining (f s)) >> pure (f s))

