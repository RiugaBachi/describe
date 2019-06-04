# describe

`describe` is a library that provides the `Descriptor s a` applicative functor for describing binary data structures. The underlying binary serialization/deserialization library is `cereal`. Instead of describing how this can be useful, I find that its usefulness is best demonstrated with the following example.

## The Problem

The problem with de/serialization libraries is that you end up having to write both the Get and Put functions yourself, even though they are almost always isomorphic.

```hs
{-# LANGUAGE RecordWildCards #-}
import Data.Serialize.Get
import Data.Serialize.Put

data Example = Example { f1 :: Word32
                         f2 :: Word64
                         f3 :: Word16
                         f4 :: Int8 }

putExample ex@Example{..} = do
  putWord32le f1
  putWord64le f2
  putWord16le f3
  putWord8 0 -- Put a dummy/unk value
  putInt8 f4

getExample = Test <$> getWord32le
                  <*> getWord64le
                  <*> getWord16le
                  <*  getWord8 -- Read the dummy/unk value
                  <*> getInt8

main = do
  let ex = Example 31 54 78 92
  let bs = runPut (putExample ex)
  Right original <- runGet getExample bs
```

If you're someone like me who develops server emulators and the like, where a game has about ~430 opcodes, defining message structures like this can quickly lead to a burnout. While other libraries exist that can derive de/serialization implementations on a whole data structure, I find that this is too limiting for my needs. Deriving a structure via generics assumes the user is in full control of the structure of the data, but when you're writing a server emulator for some Korean MMO client, many of the packet fields will be unknown, and you'll want to fill it in with some sort of arbitrary value(s) known to work and hide that from the public interface of your message GADT or whatever.

My library allows you to do something like the following:

```hs
{-# LANGUAGE GADTs, DataKinds #-}
import Data.Serialize.Descriptor
import Data.Serialize.Descriptor.LE

data PlayerTradeMessage (a :: Direction) where
  InitiatePlayerTrade :: { f1 :: Word32,
                           f2 :: Word16,
                           f3 :: Word8,
                           f4 :: Int8 } -> PlayerTradeMessage 'Serverbound

descriptor = InitiatePlayerTrade <$> w32 f1
                                 <*> w16 f2
                                 <*> w8 f3
                                 <*  w8 (const 0) -- Ignore this field when deserializing, and put a '0' during serialization.
                                 <*> i8 f4

main = do
  let message = InitiatePlayerTrade 31 46 79 81
  Right bs <- serialize message descriptor
  originalMessage <- deserialize message descriptor
```

As an added bonus, my combinator names are minimalistic (2-3 characters long), which helps churn out message structures faster.

I understand this library is somewhat niche, but hopefully others can find it useful. There is no `Monad` instance for `Descriptor s a` as it cannot be defined without running the inner `Get` per bind. I find that a monad instance isn't really ever necessary for writing message structures, so this shouldn't matter.