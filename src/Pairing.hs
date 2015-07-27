{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pairing (
      Combines(..)
    , Pairing(..)
    , CombinesM(..)
    , PairingM(..)
    , pairEffect
    , pairEffect'
    , pairEffectM
    , pairEffectM'
    ) where

import Control.Applicative
import           Control.Comonad              (Comonad, extract)
import           Control.Comonad.Trans.Cofree (CofreeT, unwrap)
import           Control.Monad.Trans.Free     (FreeF (..), FreeT, runFreeT)
import           Data.Functor.Identity        (Identity (..))
import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Cofree as Cofree

import           Sum
import           Product

import           Control.Category
import           Prelude hiding ((.),id)

class Combines a b r | a b -> r where
  combine :: a -> b -> r
  default combine :: a -> b -> (a,b)
  combine a b = (a,b)

instance {-# OVERLAPPABLE #-} (Pairing f g,Combines a b r) => Combines (f a) (g b) r where
  combine = pair

instance (Combines a b r,Combines a' b' r)
  => Combines (Either a a') (b,b') r
  where
    combine (Left a  ) (b,_ ) = combine a b
    combine (Right a') (_,b') = combine a' b'

instance (Combines a b r,Combines a' b' r)
  => Combines (a,a') (Either b b') r
  where
    combine (a,_ ) (Left b  ) = combine a b
    combine (_,a') (Right b') = combine a' b'

instance (Combines a b r,Combines a' b' r',Combines r r' x)
  => Combines (a,a') (b,b') x
  where
    combine (a,a') (b,b') =
      combine (combine a b) (combine a' b')

instance (Combines (a ar) (b br) r
         ,Combines (a' ar) (b' br) r'
         ,Combines r r' res
         )
  => Combines ((a :*: a') ar) ((b :*: b') br) res
  where
    combine (Product a a') (Product b b') =
      combine (combine a b) (combine a' b')

instance (Combines (a ar) (b br) res
         ,Combines (a' ar) (b' br) res
         )
  => Combines ((a :*: a') ar) ((b :+: b') br) res
  where
    combine (Product a _ ) (Inl b ) = combine a b
    combine (Product _ a') (Inr b') = combine a' b'

class (Functor f,Functor g) => Pairing f g where
  pair :: Combines a b r => f a -> g b -> r

instance Pairing Identity Identity where
  pair (Identity a) (Identity b) = combine a b

instance Pairing ((->) a) ((,) a) where
  pair f = uncurry $ combine . f
instance Pairing ((,) a) ((->) a) where
  pair (a,b) f = combine b (f a)

instance Pairing f g => Pairing (Cofree.Cofree f) (Free.Free g) where
  pair (a Cofree.:< _ ) (Free.Pure x ) = combine a x
  pair (_ Cofree.:< fs) (Free.Free gs) = pair fs gs
instance Pairing g f => Pairing (Free.Free g) (Cofree.Cofree f) where
  pair (Free.Pure x ) (a Cofree.:< _ ) = combine x a
  pair (Free.Free gs) (_ Cofree.:< fs) = pair gs fs

instance (Pairing f f',Pairing g g') => Pairing (f Sum.:+: g) (f' Product.:*: g') where
  pair (Inl x) (Product a _) = pair x a
  pair (Inr x) (Product _ b) = pair x b
instance (Pairing f f',Pairing g g') => Pairing (f Product.:*: g) (f' Sum.:+: g') where
  pair (Product a _) (Inl x) = pair a x
  pair (Product _ b) (Inr x) = pair b x

pairEffect
  :: (Monad m, Cofree.ComonadCofree f w, Pairing f g, Pairing w (FreeT g m),
      Combines a b1 (m b)) =>
     w a -> FreeT g m b1 -> m b
pairEffect s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> combine (extract s) x
    Free gs -> pair (unwrap s) gs

pairEffect' s c = do
  a <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> combine a x
    Free gs -> pair (unwrap s) gs

class Monad m => CombinesM a b m r where
  combineM :: a -> b -> m r

instance Monad m => CombinesM () () m () where
  combineM _ _ = return ()

instance (PairingM f g m,CombinesM a b m r) => CombinesM (f a) (g b) m r where
  combineM = pairM

class (Functor m, Functor f, Functor g) => PairingM f g m where
  pairM :: CombinesM a b m r => f a -> g b -> m r

instance Monad m => PairingM m m m where
  pairM ma mb = do
    a <- ma
    b <- mb
    combineM a b

instance (PairingM f f' m,PairingM g g' m) => PairingM (f :*: g) (f' :+: g') m where
  pairM (Product l _) (Inl l') = pairM l l'
  pairM (Product _ r) (Inr r') = pairM r r'

instance (PairingM f g m,Functor f,Functor g,Comonad w,Monad m) => PairingM (CofreeT f w) (FreeT g m) m where
  pairM = pairEffectM

pairEffectM
  :: (Cofree.ComonadCofree f w, PairingM f g m,
      PairingM w (FreeT g m) m, CombinesM a b1 m b) =>
     w a -> FreeT g m b1 -> m b
pairEffectM s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> combineM (extract s) x
    Free gs -> pairM (unwrap s) gs

pairEffectM'
  :: (Cofree.ComonadCofree f w, PairingM f g m,
      PairingM w (FreeT g m) m, CombinesM a b1 m b,
      CombinesM (m a) b1 m b) =>
     w (m a) -> FreeT g m b1 -> m b
pairEffectM' s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> combineM a x
    Free gs -> pairM (unwrap s) gs


instance (CombinesM a b m r,CombinesM a' b' m r)
  => CombinesM (Either a a') (b,b') m r
  where
    combineM (Left a  ) (b,_ ) = combineM a b
    combineM (Right a') (_,b') = combineM a' b'

instance (CombinesM a b m r,CombinesM a' b' m r)
  => CombinesM (a,a') (Either b b') m r
  where
    combineM (a,_ ) (Left b  ) = combineM a b
    combineM (_,a') (Right b') = combineM a' b'
