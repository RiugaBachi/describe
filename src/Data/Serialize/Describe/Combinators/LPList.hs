module Data.Serialize.Describe.Combinators.LPList where

import Data.Serialize.Describe
import Control.Monad

-- | A type level wrapper around a length-prefixed list combinator. The parameter @t@ is the integral descriptor to encode the length in.
newtype LPList t a 
  = LPList { unwrapLPList :: [a] }
  deriving (Show) via [a]

instance (Describe t, Describe a, Integral t) => Describe (LPList t a) where
  describe f = do
    len <- describe @t $ fromIntegral . length . unwrapLPList . f
    fmap LPList $ forM [0..fromIntegral len-1] $ \i -> describe @a $ (!! i) . unwrapLPList . f 

