{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TemplateHaskell        #-}
module Product ((:*:)(..), (*:*)) where

import           Control.Applicative (liftA2)

import           Language.Haskell.TH.Syntax

data (f :*: g) a = Product (f a) (g a)
  deriving (Functor,Show,Eq)
instance (Lift (f a), Lift (g a)) => Lift ((:*:) f g a) where
  lift (Product x y) = [| Product x y |]

infixr 7 :*:

(*:*) :: (Functor f,Functor g) => (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 Product

infixr 7 *:*
