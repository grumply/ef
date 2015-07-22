{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts       #-}
module Sum ((:+:)(..)) where

import           Language.Haskell.TH.Syntax

data (f :+: g) x = Inl (f x) | Inr (g x)
  deriving (Functor,Eq)
instance (Show (f x),Show (g x)) => Show ((:+:) f g x) where
  show (Inl fx) = show fx
  show (Inr gx) = show gx
instance (Lift (f a),Lift (g a)) => Lift ((:+:) f g a) where
  lift (Inl fa) = [| Inl fa |]
  lift (Inr ga) = [| Inr ga |]

infixr 6 :+:
