module Data.Serialize.Describe.Combinators.Byte where

import Data.Word
import Data.Int
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Class
import Data.Serialize.Describe.Isomorphisms

w8 :: Integral i => (s -> i) -> Descriptor s i
w8 = isoField @Word8 fi

i8 :: Integral i => (s -> i) -> Descriptor s i
i8 = isoField @Int8 fi
