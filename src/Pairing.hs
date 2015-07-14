-- > {-# LANGUAGE MultiParamTypeClasses #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE FunctionalDependencies #-}
-- > {-# LANGUAGE UndecidableInstances #-}
-- > module Pairing where

-- > import           Control.Comonad

-- > import qualified Control.Comonad.Cofree as Cofree
-- > import           Control.Comonad.Trans.Cofree

-- > import qualified Control.Monad.Free as Free
-- > import           Control.Monad.Trans.Free

-- > import Data.Functor.Identity

-- > class (Functor f,Functor g) => Pairing f g where
-- >   pair :: (a -> b -> r) -> f a -> g b -> r

-- > instance {-# OVERLAPPABLE #-} Pairing f g => Pairing g f where
-- >   pair p f g = pair (flip p) g f

-- > instance Pairing Identity Identity where
-- >   pair f (Identity a) (Identity b) = f a b

-- > instance {-# OVERLAPPABLE #-} Pairing ((->) a) ((,) a) where
-- >   pair p f = uncurry (p . f)

-- > instance Pairing f g => Pairing (Cofree.Cofree f) (Free.Free g) where
-- >   pair p (a Cofree.:< _) (Free.Pure x) = p a x
-- >   pair p (_ Cofree.:< fs) (Free.Free gs) = pair (pair p) fs gs

-- > pairEffect :: (Pairing f g, Comonad w, Monad m)
-- >            => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
-- > pairEffect p s c = do
-- >   mb <- runFreeT c
-- >   case mb of
-- >     Pure x -> return $ p (extract s) x
-- >     Free gs -> pair (pairEffect p) (unwrap s) gs

-- > pairEffect' :: (Pairing f g, Comonad w, Monad m)
-- >            => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
-- > pairEffect' p s c = do
-- >   a  <- extract s
-- >   mb <- runFreeT c
-- >   case mb of
-- >     Pure x -> return $ p a x
-- >     Free gs -> pair (pairEffect' p) (unwrap s) gs

-- > class (Functor m, Functor f, Functor g) => PairingM f g m | f g -> m where
-- >   pairM :: (a -> b -> m r) -> f a -> g b -> m r

-- > pairEffectM :: (PairingM f g m, Comonad w, Monad m)
-- >             => (a -> b -> m r) -> CofreeT f w a -> FreeT g m b -> m r
-- > pairEffectM p s c = do
-- >   mb <- runFreeT c
-- >   case mb of
-- >     Pure x -> p (extract s) x
-- >     Free gs -> pairM (pairEffectM p) (unwrap s) gs

-- > pairEffectM' :: (PairingM f g m,Comonad w,Monad m)
-- >              => (a -> b -> m r) -> CofreeT f w (m a) -> FreeT g m b -> m r
-- > pairEffectM' p s c = do
-- >   a  <- extract s
-- >   mb <- runFreeT c
-- >   case mb of
-- >     Pure x -> p a x
-- >     Free gs -> pairM (pairEffectM' p) (unwrap s) gs
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Pairing (
      Pairing(..)
    , pairEffect
    , pairEffect'
    , PairingM(..)
    , pairEffectM
    ) where

import           Control.Comonad              (Comonad, extract)
import           Control.Comonad.Trans.Cofree (CofreeT, unwrap)
import           Control.Monad.Trans.Free     (FreeF (..), FreeT, runFreeT)
import           Data.Functor.Identity        (Identity (..))

class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

pairEffect :: (Pairing f g, Functor f, Functor g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p (extract s) x
    Free gs -> pair (pairEffect p) (unwrap s) gs

pairEffect' :: (Pairing f g, Functor f, Functor g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
pairEffect' p s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p a x
    Free gs -> pair (pairEffect' p) (unwrap s) gs

class (Functor m, Functor f, Functor g) => PairingM f g m | f g -> m where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

pairEffectM :: (PairingM f g m, Comonad w, Monad m)
            => (a -> b -> m r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffectM p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> p (extract s) x
    Free gs -> pairM (pairEffectM p) (unwrap s) gs
