{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures #-}
module Pairing (
      Combines(..)
    , Pairing(..)
    , pairEffect
    , pairEffect'
    ) where

import Control.Monad.Trans.Free
import Control.Comonad.Trans.Cofree
import Control.Comonad

import qualified Control.Comonad.Cofree as Cofree
import qualified Control.Monad.Free as Free

import           Sum
import           Product

import           Control.Category
import           Prelude hiding ((.),id)

import Data.Functor.Identity

class Combines a b r | a b -> r where
  combine :: a -> b -> r

instance Combines () () () where
  combine _ _ = ()

instance (Pairing f g,Combines a b r) => Combines (f a) (g b) r where
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

class Pairing f g | f -> g, g -> f where
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
instance (Pairing f' f,Pairing g' g) => Pairing (f' Product.:*: g') (f Sum.:+: g) where
  pair (Product a _) (Inl x) = pair a x
  pair (Product _ b) (Inr x) = pair b x

pairEffect
  :: (Monad m, ComonadCofree f w, Pairing f g, Pairing w (FreeT g m),
      Combines a b1 (m b)) =>
     w a -> FreeT g m b1 -> m b
pairEffect s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> combine (extract s) x
    Free gs -> pair (unwrap s) gs

pairEffect'
  :: (Monad m, ComonadCofree f w, Pairing f g, Pairing w (FreeT g m),
      Combines a b1 (m b), Combines (m a) b1 (m b)) =>
     w (m a) -> FreeT g m b1 -> m b
pairEffect' s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> combine a x
    Free gs -> pair (unwrap s) gs
