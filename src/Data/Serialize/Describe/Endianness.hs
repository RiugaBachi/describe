module Data.Serialize.Describe.Endianness(
  LE(..),
  BE(..)
) where

newtype LE a = LE { unwrapLE :: a }

newtype BE a = BE { unwrapBE :: a }

