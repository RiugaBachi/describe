module Data.Serialize.Descriptor(
    Descriptor(Descriptor),
    unwrapGet,
    unwrapPut,
    serialize,
    deserialize
) where

import Data.Word
import Data.ByteString (ByteString)
import Data.Serialize.Get
import Data.Serialize.Put

newtype Descriptor s a = Descriptor {
    unwrapDescriptor :: (Get a, s -> Put)
}

unwrapGet :: Descriptor s a -> Get a
unwrapGet = fst . unwrapDescriptor

unwrapPut :: s -> Descriptor s a -> Put
unwrapPut s = ($ s) . snd . unwrapDescriptor

serialize :: s -> Descriptor s a -> ByteString
serialize s = runPut . unwrapPut s

deserialize :: ByteString -> Descriptor s s -> Either String s
deserialize bs = flip runGet bs . unwrapGet

instance Functor (Descriptor s) where
  fmap f (Descriptor (g, p)) = Descriptor (f <$> g, p)

instance Applicative (Descriptor s) where
  pure a = Descriptor (pure a, \_ -> pure ())
  (Descriptor (f, p)) <*> (Descriptor (g, p')) =
    Descriptor (f <*> g, \s' -> p s' >> p' s')
