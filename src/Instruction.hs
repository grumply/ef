{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
module Instruction where

import Sum
import Subsumption
import Pairing
import Product

import Control.Comonad
import Control.Comonad.Trans.Cofree

import Data.Proxy

class (Functor f) => Point f where
  point :: a -> f a

class (Functor f) => Build a f b where
  build :: a -> f b

class (Functor f,Functor g) => Transform f g where
  transform :: f a -> g a

class (Functor f,Functor g) => Embed f g where
  embed :: f a -> g (f a)

instance Point f => Build a f a where
  build = point

instance (Embed w g, Embed w h)
    => Embed w ((:*:) g h)
  where embed a = Product (embed a) (embed a)


-- embed with a witness to a Pairing
class (Pairing g f,Embed w g,Functor f,Functor g) => Presume f g w where
  presume' :: Proxy (f b) -> w a -> g (w a)

instance (Comonad w,Pairing g f,Embed w g)
    => Presume f g w
  where
    presume' _ = embed

embed' :: (Comonad w,Pairing g f,Embed w g) => w a -> g (w a)
embed' = presume' (Proxy :: Proxy (f b))

computer :: (Comonad w,Monad m,Pairing f g, Embed w f)
         => w (m a) -> CofreeT f w (m a)
computer = coiterT embed'

type (:<>:) f g w = Presume f g w

infixl 5 :<>:
