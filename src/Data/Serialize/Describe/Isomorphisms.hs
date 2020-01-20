module Data.Serialize.Describe.Isomorphisms where

import Control.Lens.Iso
import Data.Char

fi :: (Integral i1, Integral i2) => Iso' i1 i2
fi = iso fromIntegral fromIntegral

rtf :: (Real i1, Fractional i1, Real i2, Fractional i2) => Iso' i1 i2
rtf = iso realToFrac realToFrac

ordChr :: Iso' Char Int
ordChr = iso ord chr
