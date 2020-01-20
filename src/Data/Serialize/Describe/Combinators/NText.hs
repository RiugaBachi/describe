module Data.Serialize.Describe.Combinators.NText where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Trans.Class
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Serialize.Describe.Internal.Descriptor
import Data.Serialize.Describe.Class

-- | A null-terminated text descriptor which reads characters until '\0' is encountered. Semantically, the '\0' is consumed by the parser. The entire text will be serialized with a '\0' character appended to the end.
ntext :: (MonadTrans m, forall x. Monad x => Monad (m x)) => (s -> Text) -> DescriptorM m s Text
ntext f = T.pack . reverse <$> go 0 []
  where
    go i cs = field @Char (test i . f) >>= \case
      '\0' -> pure cs
      c -> pure (c:cs)

    test i t | i >= T.length t = '\0'
             | otherwise = T.index t i

-- | Type-level variant of @ftext@.
newtype NText = NText { unwrapNText :: Text }
                deriving (Show) via Text

instance IsString NText where
  fromString = NText . T.pack

instance Describe NText where
  describe = 
    NText <$> ntext unwrapNText

