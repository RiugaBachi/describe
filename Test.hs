module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Word
import Data.Either
import qualified Data.ByteString as B
import Data.Serialize.Descriptor
import Data.Serialize.Descriptor.LE

data Test = Test { f1 :: Word8, f2 :: Word16, f3 :: Word32, f4 :: Word8 }
          deriving (Eq)

prop_identity :: Bool
prop_identity =
  either (const False) (== struct) $ deserialize (serialize struct desc) desc
  where
    struct = Test 42 63 78 31
    desc = Test <$> w8 f1 <*> w16 f2 <*> w32 f3 <*> w8 f4 

main :: IO ()
main = do
  quickCheck prop_identity
