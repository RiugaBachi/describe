module Data.Serialize.Describe.Descriptor(
  Descriptor(Descriptor),
  unwrapGet,
  unwrapPut,
  serialize,
  deserialize,
  deserializeEx
) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Serialize.Get
import Data.Serialize.Put

-- | @Descriptor s a@ is an applicative functor that describes the binary structure for a structure 's' while deserializing value 'a'.
newtype Descriptor s a = Descriptor {
    unwrapDescriptor :: (Get a, s -> PutM a)
}

-- | @unwrapGet desc@ takes a 'Descriptor' and returns only the internal 'Get' monad.
unwrapGet :: Descriptor s a -> Get a
unwrapGet = fst . unwrapDescriptor

-- | @unwrapPut s desc@ takes the structure being described and a 'Descriptor' for it, and returns the internal 'Put' monad.
unwrapPut :: s -> Descriptor s a -> PutM a
unwrapPut s = ($ s) . snd . unwrapDescriptor

-- | Convenience function for @runPut . unwrapPut s@
serialize :: s -> Descriptor s a -> ByteString
serialize s = snd . runPutM . unwrapPut s

-- | Convenience function for @flip runGet bs . unwrapGet@
deserialize :: ByteString -> Descriptor s s -> Either String s
deserialize bs = flip runGet bs . unwrapGet

newtype ParserException = ParserException String
                        deriving (Show)

instance Exception ParserException

-- | Like 'deserialize', but throw a 'ParserException' upon failure rather than an `Either`.
deserializeEx :: ByteString -> Descriptor s s -> s
deserializeEx bs d = case deserialize bs d of
  Left err -> throw $ ParserException err
  Right a -> a

instance Functor (Descriptor s) where
  fmap f (Descriptor (g, p)) = Descriptor (f <$> g, (f <$>) . p)

instance Applicative (Descriptor s) where
  pure a = Descriptor (pure a, \_ -> pure a)
  (Descriptor (f, p)) <*> (Descriptor (g, p')) =
    Descriptor (f <*> g, \s' -> p s' <*> p' s')

instance Monad (Descriptor s) where
  (Descriptor (g, p)) >>= f =
    Descriptor (g >>= fst . unwrapDescriptor . f, \s -> p s >>= ($ s) . snd . unwrapDescriptor . f)

