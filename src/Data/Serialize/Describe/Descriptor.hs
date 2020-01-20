module Data.Serialize.Describe.Descriptor(
  DescriptorM,
  Descriptor,
  -- * Execution
  serialize,
  deserialize,
  deserializeEx,
  -- * Conversion to @cereal@ monads
  unwrapGet,
  unwrapPut,
  -- * Utilities
  morphRef,
  morphTransformer,
  isolate,
  conditionally,
  cursor
) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Serialize.Get hiding (isolate)
import Data.Serialize.Put
import Data.Serialize.Describe.Internal.Descriptor
import Control.Monad.Trans.Identity

-- | Convenience function for @runPut . unwrapPut s@
serialize :: s -> Descriptor s a -> ByteString
serialize s = snd . runPutM . runIdentityT . unwrapPut s

-- | Convenience function for @flip runGet bs . unwrapGet@
deserialize :: ByteString -> Descriptor s s -> Either String s
deserialize bs = flip runGet bs . runIdentityT . unwrapGet

newtype ParserException = ParserException String
                        deriving (Show)

instance Exception ParserException

-- | Like 'deserialize', but throw a 'ParserException' upon failure rather than an `Either`.
deserializeEx :: ByteString -> Descriptor s s -> s
deserializeEx bs d = case deserialize bs d of
  Left err -> throw $ ParserException err
  Right a -> a
