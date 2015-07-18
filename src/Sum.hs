{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts       #-}
module Sum ((:+:)(..)) where

import           Language.Haskell.TH.Syntax

data (f :+: g) x = Inl (f x) | Inr (g x)
  deriving (Functor,Show,Eq)
instance (Lift (f a),Lift (g a)) => Lift ((:+:) f g a) where
  lift (Inl fa) = [| Inl fa |]
  lift (Inr ga) = [| Inr ga |]

infixr 6 :+:
