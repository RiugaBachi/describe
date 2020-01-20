module Data.Serialize.Describe.Internal.Descriptor where

import Data.Bool
import Data.Profunctor
import Data.Serialize.Get hiding (isolate)
import qualified Data.Serialize.Get as G
import Control.Lens
import Data.Serialize.Put
import Control.Monad.Trans.Identity
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Morph

-- | @Descriptor s a@ is a monad that describes the binary structure for a structure 's' while deserializing value 'a'.
newtype DescriptorM m s a 
  = Descriptor { unwrapDescriptor :: (StateT Int (m Get) a, s -> StateT Int (m PutM) a) }

type Descriptor s a = DescriptorM IdentityT s a

instance (MonadTrans m, forall x. Monad x => Monad (m x)) => Functor (DescriptorM m s) where
  fmap f (Descriptor (g, p)) = Descriptor (f <$> g, fmap f <$> p)

instance (MonadTrans m, forall x. Monad x => Monad (m x)) => Profunctor (DescriptorM m) where
  lmap = morphRef
  rmap = fmap
  dimap l r = morphRef l . fmap r

instance (MonadTrans m, forall x. Monad x => Monad (m x)) => Applicative (DescriptorM m s) where
  pure a = Descriptor (pure a, \_ -> pure a)
  (Descriptor (f, p)) <*> (Descriptor (g, p')) =
    Descriptor (f <*> g, \s -> p s <*> p' s)

instance (MonadTrans m, forall x. Monad x => Monad (m x)) => Monad (DescriptorM m s) where
  (Descriptor (g, p)) >>= f =
    Descriptor (g >>= bindG , \s -> p s >>= bindP s)
    where
      bindG x = fst $ unwrapDescriptor (f x)
      bindP s x = ($ s) . snd $ unwrapDescriptor (f x)

mkDescriptor :: (MonadTrans m, forall x. Monad x => Monad (m x)) 
             => Iso' a s 
             -> (s -> Int) 
             -> Get s 
             -> (s -> Put) 
             -> DescriptorM m a a
mkDescriptor i size g p = 
  Descriptor $ (,) 
    (view (from i) <$> ((lift . lift) g >>= \s' -> modify (+ size s') >> pure s'))
    (\s -> modify (+ size (view i s)) >> (lift . lift) (p $ view i s) >> pure s)

-- | @unwrapGet desc@ takes a 'Descriptor' and returns only the internal 'Get' monad.
unwrapGet :: (MonadTrans m, forall x. Monad x => Monad (m x)) => DescriptorM m s a -> m Get a
unwrapGet = flip evalStateT 0 . fst . unwrapDescriptor

-- | @unwrapPut s desc@ takes the structure being described and a 'Descriptor' for it, and returns the internal 'Put' monad.
unwrapPut :: (MonadTrans m, forall x. Monad x => Monad (m x)) => s -> DescriptorM m s a -> m PutM a
unwrapPut s = flip evalStateT 0 . ($ s) . snd . unwrapDescriptor

-- | Shifts the reference point of the described structure using the provided morphism. Identical to @lmap@.
morphRef 
  :: ( MonadTrans m
     , forall x. Monad x => Monad (m x)
     ) 
  => (s -> t) 
  -> DescriptorM m t a 
  -> DescriptorM m s a
morphRef f (Descriptor (g, p)) = Descriptor (g, p . f)

-- | Morphs the underlying monad transformer
morphTransformer 
  :: ( MonadTrans t
     , MonadTrans u
     , forall n. Monad n => (Monad (t n), Monad (u n))
     )
  => (forall m x. Monad m => t m x -> u m x)
  -> DescriptorM t s a
  -> DescriptorM u s a
morphTransformer f (Descriptor (g, p)) = Descriptor (hoist f g, hoist f <$> p)

-- | Wrapper around @isolate@ from Data.Serialize.Get
isolate 
  :: ( MonadTransControl m
     , forall x. Monad x => Monad (m x)
     ) 
  => Int 
  -> DescriptorM m s a 
  -> DescriptorM m s a
isolate amt desc = 
  Descriptor 
    (liftWith (\r' -> 
      liftWith (\r -> G.isolate amt $ r . r' . fst $ unwrapDescriptor desc) 
        >>= restoreT . return) 
          >>= restoreT . return, snd $ unwrapDescriptor desc)

-- | Similar to lookAhead from Data.Serialize.Get; puts only if the field is @Just@
conditionally 
  :: ( MonadTransControl m
     , forall x. Monad x => Monad (m x)
     )
  => (s -> Maybe a)
  -> (a -> Bool)
  -> DescriptorM m a a 
  -> DescriptorM m s (Maybe a)
conditionally f pd desc = Descriptor (g, p)
  where
    g = 
      liftWith (\r' -> 
        liftWith (\r -> G.lookAhead (r . r' . fst $ unwrapDescriptor desc)) 
          >>= restoreT . return) 
            >>= restoreT . return 
              >>= bool (pure Nothing) (Just <$> fst (unwrapDescriptor desc)) . pd
    p ((f $) -> Just x) = bool 
      (pure Nothing) 
      (fmap Just . ($ x) . snd  . unwrapDescriptor $ desc) 
      (pd x)
    p _ = pure Nothing

-- | The cursor position, i.e. how many bytes have been written/parsed so far.
cursor :: (MonadTrans m, forall x. Monad (m x)) => DescriptorM m s Int
cursor = Descriptor (get, \_ -> get)
