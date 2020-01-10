module Data.Serialize.Describe.Combinators.Isolate where

import qualified Data.Serialize.Get as G
import Data.Serialize.Describe.Descriptor

-- | Wrapper around @isolate@ from Data.Serialize.Get
isolate :: Int -> Descriptor s a -> Descriptor s a
isolate amt desc = Descriptor (G.isolate amt $ unwrapGet desc, flip unwrapPut desc)
