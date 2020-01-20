module Data.Serialize.Describe.Combinators.LPList where

import Data.Serialize.Describe
import Control.Monad

-- | A type level wrapper around a length-prefixed list combinator. The parameter @t@ is the integral descriptor to encode the length in.
newtype LPList t a 
  = LPList { unwrapLPList :: [a] }
  deriving (Show) via [a]

instance (Describe t, Describe a, Integral t) => Describe (LPList t a) where
  type Context m (LPList t a) = (Context m a, Context m t)
  describe = do
    len <- field @t $ fromIntegral . length . unwrapLPList
    fmap LPList $ forM [0..fromIntegral len-1] $ \i -> field @a $ (!! i) . unwrapLPList

