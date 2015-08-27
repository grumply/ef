{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Instruction where
import Pairing
import Product

import Control.Comonad
import Control.Comonad.Trans.Cofree

import Data.Proxy

class (Functor w,Functor g) => Buildable g w where
  build :: w a -> g (w a)

instance (Buildable g w, Buildable h w)
    => Buildable ((:*:) g h) w
  where build a = Product (build a) (build a)

class (Buildable g w,Functor f,Functor g) => Presume f g w where
  presume' :: Proxy (f b) -> w a -> g (w a)

instance (Comonad w ,Pairing g f,Buildable g w)
    => Presume f g w
  where
    presume' _ = build

build' :: (Comonad w,Pairing g f,Buildable g w) => w a -> g (w a)
build' = presume' (Proxy :: Proxy (f b))


computer :: (Comonad w,Monad m,Pairing f g, Buildable f w)
         => w (m a) -> CofreeT f w (m a)
computer = coiterT build'

type (:<>:) f g w = Presume f g w

infixl 5 :<>:
