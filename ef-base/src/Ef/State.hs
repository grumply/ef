module Ef.State (State(..),state,get,put,modify,lput,lmodify) where

import Ef

import Control.Lens hiding (Strict)

data Eagerness = NonStrict | Strict

data State s k
  = State s k (s -> k)
  | Get (s -> k)
  | Put Eagerness s k
  | Modify Eagerness (s -> s) k

instance Ma (State s) (State s) where
  ma use (State s k _) (Get sk) =
    let k' = sk s
    in use k k'
  ma use (State _ _ sk) (Put Strict !s k') =
    let k = sk s
    in use k k'
  ma use (State _ _ sk) (Put NonStrict s k') =
    let k = sk s
    in use k k'
  ma use (State s _ sk) (Modify Strict ss k') =
    let !s' = ss s
    in use (sk s') k'
  ma use (State s _ sk) (Modify NonStrict ss k') =
    let s' = ss s
    in use (sk s') k'

state :: forall self st super traits.
         (Monad super, '[State st] <. traits)
      => st -> Trait (State st) traits super
state initial = State initial return $! \new -> pure . set trait (state new)
{-# INLINE state #-}

get = self (Get id)

put new = self (Put Strict new ())

modify f = self (Modify Strict f ())

lput new = self (Put NonStrict new ())

lmodify f = self (Modify NonStrict f ())
