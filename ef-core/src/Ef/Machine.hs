{-# language FlexibleContexts #-}
module Ef.Machine where

import Ef.Narrative
import Ef.Object
import Ef.Ma
import Ef.Messages
import Ef.Traits

import Control.Monad.Catch

import Control.Comonad
import Control.Comonad.Cofree

-- Like Turing's machine, this module unifies narratives, or instructions, with
-- a computer of said instructions into an infinite stream of steps of
-- instructions and computers transformed by those instructions.

-- These are exceptionally useful for simulation and implemented nearly trivally.

path :: (Monad super, Ma (Traits traits) (Messages self))
      => Object traits super
      -> Narrative self super a
      -> Cofree super (Object traits super, Narrative self super a)
path obj nar = coiter (uncurry lay) (obj,nar)
  where
    lay obj = go
      where
        go (Say symbol k) =
          let (method,b) = ma (,) (deconstruct obj) symbol
          in do object' <- method obj
                return (object',k b)
        go (Super m)  = do
          nar <- m
          return (obj,nar)
        go done = return (obj,done)

eval :: (Monad super, MonadThrow super, Ma (Traits traits) (Messages self))
     => Cofree super (Object traits super, Narrative self super a)
     -> super (Object traits super,a)
eval machine = do
  next <- unwrap machine
  let (obj,nar) = extract next
  case nar of
    (Return r) -> return (obj,r)
    (Fail f)   -> throwM f
    _          -> eval next

swapObj :: (Monad super, MonadThrow super, Ma (Traits traits) (Messages self))
        => Object traits super
        -> Cofree super (Object traits super, Narrative self super a)
        -> Cofree super (Object traits super, Narrative self super a)
swapObj obj = path obj . snd . extract

swapNar :: (Monad super, MonadThrow super, Ma (Traits traits) (Messages self))
        => Narrative self super a
        -> Cofree super (Object traits super, Narrative self super a)
        -> Cofree super (Object traits super, Narrative self super a)
swapNar nar = flip path nar . fst . extract

branch :: (Monad super, MonadThrow super, Ma (Traits traits) (Messages self))
       => Cofree super (Object traits super, Narrative self super a)
       -> Cofree super (Cofree super (Object traits super, Narrative self super a))
branch = duplicate

reseedObj :: (Monad super, MonadThrow super, Ma (Traits traits) (Messages self))
          => Object traits super
          -> Cofree super (Cofree super (Object traits super, Narrative self super a))
          -> Cofree super (Cofree super (Object traits super, Narrative self super a))
reseedObj obj = fmap (swapObj obj)

reseedNar :: (Monad super, MonadThrow super, Ma (Traits traits) (Messages self))
          => Narrative self super a
          -> Cofree super (Cofree super (Object traits super, Narrative self super a))
          -> Cofree super (Cofree super (Object traits super, Narrative self super a))
reseedNar nar = fmap (swapNar nar)
