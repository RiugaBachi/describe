module Data.Serialize.Describe.Class(
  Describe(..),
  describeVia,
  field, isoField
) where

import GHC.Generics
import GHC.Exts
import GHC.TypeNats
import Language.Haskell.TH
import Data.Profunctor
import Control.Monad
import qualified Data.Vector.Fixed as V
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Vector.Fixed.Boxed (Vec)
import Data.Char
import Data.Int
import Data.Proxy
import Data.Word
import Control.Lens (view)
import Control.Lens.Iso (Iso')
import qualified Control.Lens.Iso as I
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Data.Serialize.Describe.Internal.Descriptor
import Data.Serialize.Describe.Isomorphisms

class Describe a where
  {-# MINIMAL #-}
  type Context (m :: (* -> *) -> * -> *) a :: Constraint 
  type Context m a = () 

  describe :: (MonadTrans m, forall x. Monad x => Monad (m x), Context m a) => DescriptorM m a a

  default describe :: ( Generic a
                      , GDescribe (Rep a)
                      , MonadTrans m
                      , forall x. Monad x => Monad (m x)
                      ) => DescriptorM m a a 
  describe = morphTransformer (lift . runIdentityT) $ dimap from to gdescribe

-- | An alternative to DerivingVia, as the type variable @a@ of @Describe@ is bound to the type family @Context@ making its role @nominal@, and subsequently, uncoercible as per the semantics of DerivingVia. A restriction exists, however: both of the types specified must be @Coercible@.
describeVia 
  :: Name 
  -- ^ The type wrapper to inherit @describe@ from; must be of type (* -> *)
  -> Name
  --  ^ The type to create a @Describe@ instance for
  -> Q [Dec]
describeVia wrp dst =
  [d| 
    instance Describe $destination where
      type Context m $destination = Context m ($wrapper $destination)
      describe = lmap coerce coerce describe
  |]
  where
    wrapper = conT wrp
    destination = conT dst

-- | A descriptor from structure to field.
field 
  :: forall a m s. 
      ( Describe a
      , MonadTrans m
      , forall x. Monad x => Monad (m x)
      , Context m a
      )
  => (s -> a) 
  -> DescriptorM m s a
field f = morphRef f describe

-- | Similar to @field@, but applied to an isomorphism.
isoField :: (Describe b, MonadTrans m, forall x. Monad x => Monad (m x), Context m b) 
         => Iso' a b 
         -> (s -> a) 
         -> DescriptorM m s a
isoField i f = view (I.from i) <$> morphRef (view i . f) describe

class GDescribe f where
  gdescribe :: Descriptor (f a) (f a)
  
instance GDescribe U1 where
  gdescribe = pure U1

instance (GDescribe a, GDescribe b) => GDescribe (a :*: b) where
  gdescribe = liftM2 (:*:) (lmap l gdescribe) (lmap r gdescribe)
    where
      l (a :*: _) = a
      r (_ :*: b) = b

instance (GDescribe a) => GDescribe (M1 i c a) where
  gdescribe = M1 <$> lmap extract gdescribe
    where
      extract (M1 x) = x

instance (Describe a, Context IdentityT a) => GDescribe (K1 i a) where
  gdescribe = K1 <$> lmap extract describe
    where
      extract (K1 x) = x

instance Describe () where
  describe = pure ()

instance Describe Bool where
    describe = toEnum . fromIntegral <$> field (fromIntegral @_ @Word8 . fromEnum)

instance Describe Char where
    describe = chr . fromIntegral <$> field @Word8 (fromIntegral . ord)

instance Describe Word8 where
  describe = mkDescriptor fi (const 1) getWord8 putWord8

instance Describe Int8 where
    describe = mkDescriptor fi (const 1) getWord8 putWord8

instance 
  ( Describe a
  , V.Arity n
  , V.Vector (Vec n) a
  , KnownNat n
  ) => Describe (Vec n a) where
    type Context m (Vec n a) = Context m a
    describe =
       V.fromList <$> forM [0..fromIntegral (natVal (Proxy :: Proxy n))-1] 
        (\i -> field (V.! i))
