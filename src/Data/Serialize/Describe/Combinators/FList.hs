module Data.Serialize.Describe.Combinators.FList where

import GHC.TypeNats
import Data.Proxy
import Data.Serialize.Describe
import Data.Word
import Data.Serialize.Describe.Combinators.Const
import Data.Serialize.Describe.Combinators.BE
import Data.Serialize.Describe.Combinators.LE
import Control.Monad

-- | A fixed-length list combinator, with similar semantics to @FText@. The list element must be @Nullable@, meaning that it must have default value that can be used for padding the list if need be.
flist :: (Describe a, Nullable a) => Int -> (s -> [a]) -> Descriptor s [a]
flist fixedLen f = 
  forM [0..fixedLen - 1] $ \i -> 
    describe $ \l ->
      let actualLen = length . take fixedLen $ f l
       in (!! i) . (<> replicate (fixedLen - actualLen) nullVal) $ f l

-- | A type level wrapper around @flist@.
newtype FList (n :: Nat) a 
  = FList { unwrapFList :: [a] }
  deriving (Show) via [a]

class Nullable a where
  nullVal :: a

instance Num n => Nullable n where
  nullVal = 0

deriving via n instance Num n => Nullable (LE n)
deriving via n instance Num n => Nullable (BE n)

instance Nullable (Const n a) where
  nullVal = Const

instance (KnownNat n, Nullable a, Describe a) => Describe (FList n a) where
  describe f = do
    FList <$> flist (fromIntegral (natVal $ Proxy @n)) (unwrapFList . f)
