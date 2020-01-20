module Data.Serialize.Describe.Combinators.Remaining where

import GHC.Generics
import Control.Lens.Wrapped
import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import Data.Serialize.Describe.Internal.Descriptor
import Data.Serialize.Describe.Class

-- | A  'Remaining' represents the rest of the buffer. Upon serialization, the entire wrapped ByteString will be written.
newtype Remaining 
  = Remaining { unwrapRemaining :: ByteString }
  deriving (Generic)

deriving anyclass instance Wrapped Remaining

instance Describe Remaining where
  describe = mkDescriptor _Wrapped' B.length (G.getByteString =<< G.remaining) P.putByteString

