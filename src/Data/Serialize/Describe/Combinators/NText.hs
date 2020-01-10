module Data.Serialize.Describe.Combinators.NText where

import Data.Char
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Serialize.Describe.Descriptor
import Data.Serialize.Describe.Class
import Data.Serialize.Put
import Control.Monad

-- | A null-terminated text descriptor which reads characters until '\0' is encountered. Semantically, the '\0' is consumed by the parser. The entire text will be serialized with a '\0' character appended to the end.
ntext :: (s -> Text) -> Descriptor s Text
ntext f = Descriptor (g, p)
  where
    g = unwrapGet $ T.pack . reverse <$> go []
      where
        go cs = describe @Char (const '\0') >>= \case
          '\0' -> pure cs
          c -> go $ c:cs
    p s = do
      forM_ (T.unpack (f s)) $ putWord8 . fromIntegral . ord
      putWord8 $ fromIntegral $ ord '\0'
      pure (f s)

-- | Type-level variant of @ftext@.
newtype NText = NText { unwrapNText :: Text }
                deriving (Show) via Text

instance IsString NText where
  fromString = NText . T.pack

instance Describe NText where
  describe f = 
    NText <$> ntext (fmap unwrapNText f)

