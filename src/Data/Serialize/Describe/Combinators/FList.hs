module Data.Serialize.Describe.Combinators.FList where

import GHC.TypeNats
import Data.Proxy
import Data.Serialize.Describe
import Data.Word
import Data.Serialize.Describe.Combinators.Const
import Control.Monad

-- | A type level wrapper around a fixed-length list combinator, with similar semantics to @FText@. The list element must be @Nullable@, meaning that it must have default value that can be used for padding the list if need be.
newtype FList (n :: Nat) a 
  = FList { unwrapFList :: [a] }
  deriving (Show) via [a]

class Nullable a where
  nullVal :: a

instance Nullable Word16 where
  nullVal = 0

instance Nullable (Const n a) where
  nullVal = Const

instance (KnownNat n, Nullable a, Describe a) => Describe (FList n a) where
  describe f = do
    let fixedLen = fromIntegral (natVal $ Proxy @n)
    fmap FList . forM [0..fixedLen - 1] $ \i -> 
      describe $ \l ->
        let actualLen = length . take fixedLen . unwrapFList $ f l
         in (!! i) . (<> replicate (fixedLen - actualLen) (nullVal @a)) . unwrapFList $ f l

