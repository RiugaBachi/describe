module Data.Serialize.Describe.Combinators.Byte where

import Data.Word
import Data.Int
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Class

w8 :: Integral i => (s -> i) -> Descriptor s i
w8 f = fromIntegral <$> describe @Word8 (fromIntegral . f)

i8 :: Integral i => (s -> i) -> Descriptor s i
i8 f = fromIntegral <$> describe @Int8 (fromIntegral . f)
