module Data.Serialize.Describe.Combinators.ByteEnum where

import GHC.Generics hiding (from)
import Control.Lens.Wrapped
import Control.Lens.Iso
import Data.Word
import Data.Serialize.Describe.Class
import Data.Serialize.Describe.Isomorphisms

-- | Wraps an @Enum@ to be described as a Word8. Intended to be used with DerivingVia so as to not introduce unnecessary newtype wrappers:
-- > data MyEnum = A | B | C 
-- >             deriving Enum
-- >             deriving Describe via ByteEnum MyEnum
newtype ByteEnum e 
  = ByteEnum { unwrapByteEnum :: e }
  deriving (Generic)

deriving anyclass instance Wrapped (ByteEnum e)

instance Enum e => Describe (ByteEnum e) where
  describe = isoField @Word8 (_Wrapped' . from enum . fi) id

