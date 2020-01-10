module Data.Serialize.Describe.Combinators.ByteEnum where

import Data.Word
import Data.Serialize.Describe.Class

-- | Wraps an @Enum@ to be described as a Word8. Intended to be used with DerivingVia so as to not introduce unnecessary newtype wrappers:
-- @
--  data MyEnum = A | B | C 
--              deriving Enum
--              deriving Describe via ByteEnum MyEnum
-- @
newtype ByteEnum e = ByteEnum { unwrapByteEnum :: e }

instance Enum e => Describe (ByteEnum e) where
  describe f = ByteEnum . toEnum . fromIntegral <$> describe @Word8 (fromIntegral . fromEnum . unwrapByteEnum . f)

