module Ef.Var
    ( Var(..)
    , var
    , var'
    ) where

import Ef
import Ef.Sync

data Eagerness = Strict | Lazy
  deriving Eq

data Modification state = Modify Eagerness (state -> state)

data Var var self super = Var
  { alter :: (var -> var) -> Ef self super ()
  , alter' :: (var -> var) -> Ef self super ()
  , peek :: Ef self super var
  , peeks :: forall a. (var -> a) -> Ef self super a
  , poke :: var -> Ef self super ()
  , pokes :: forall a. (a -> var) -> a -> Ef self super ()
  }

stateful :: ('[Sync] <: ms, Monad c)
         => (Var s ms c -> Ef ms c r)
         -> Synchronized (Modification s) s () X ms c r
stateful computation =
  let
    stateInterface up = Var
      { alter = \mod -> do
          _ <- up (Modify Lazy mod)
          return ()
      , alter' = \mod -> do
          _ <- up (Modify Strict mod)
          return ()
      , peek = up (Modify Lazy id)
      , peeks = \view -> do
          current <- up (Modify Lazy id)
          return (view current)
      , poke = \new -> do
          let update = const new
          up (Modify Lazy update)
          return ()
      , pokes = \view big -> do
          let new = const (view big)
          _ <- up (Modify Lazy new)
          return ()
      }

  in
      synchronized $ \up _ -> computation (stateInterface up)
{-# INLINE stateful #-}

var :: ('[Sync] <: ms, Monad c) => s -> (Var s ms c -> Ef ms c r) -> Ef ms c r
var initial computation = runSync (serve +>> stateful computation)
  where
    serve firstRequest = synchronized $ \_ dn -> do
      r <- withRespond dn initial firstRequest
      return r
      where
        withRespond respond = handle
          where
            handle current (Modify strictness mod) = do
              let new = mod current
                  force = new `seq` return ()
              case strictness of
                Strict -> force
                _ -> return ()
              next <- respond new
              handle new next
{-# INLINE var #-}

var' :: ('[Sync] <: ms, Monad c) => s -> (Var s ms c -> Ef ms c r) -> Ef ms c r
var' initial computation = runSync (serve +>> stateful computation)
  where
    serve firstRequest = synchronized $ \_ dn -> withRespond dn initial firstRequest
      where
        withRespond respond = handle
          where
            handle !current (Modify _ mod) = do
              let new = mod current
              next <- respond new
              handle new next
{-# INLINE var' #-}
