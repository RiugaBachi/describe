module Data.Serialize.Describe.Combinators.FText where

import GHC.TypeNats
import Data.Word
import Data.Char
import Data.String
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Class
import Control.Monad
import Control.Monad.Trans.Class

-- | A fixed text descriptor which reads a fixed amount of bytes, discarding all trailing '\0' characters. Upon serializing, the text will either be truncated to the specified fixed length, or padded with '\0' characters to meet it.
ftext :: (MonadTrans m, forall x. Monad x => Monad (m x)) => Int -> (s -> Text) -> DescriptorM m s Text
ftext maxLen f = 
  fmap (T.takeWhile (/= '\0') . fromString . (fmap (chr . fromIntegral))) <$> forM [0..maxLen-1] $ \i -> field $ \s -> 
    let t = f s
        p = (<> T.replicate (maxLen - T.length t) "\0") . T.take maxLen $ t
     in fromIntegral @_ @Word8 . ord $ T.index p i

-- | Type-level variant of @ftext@.
newtype KnownNat n => FText n = FText { unwrapFText :: Text }
                              deriving (Show) via Text

instance KnownNat n => IsString (FText n) where
  fromString = FText . T.pack

instance KnownNat n => Describe (FText n) where
  describe = 
    FText <$> ftext (fromIntegral (natVal (Proxy :: Proxy n))) unwrapFText

