{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Notate
    ( Noting, note
    , Notatable
    , notator
    , writer
    , noted
    ) where



import Ef.Core

import Data.Binary
import Data.Monoid



data Noting r k
  where

    Noting
        :: r
        -> k
        -> Noting r k



note
    :: Is (Noting w) fs m
    => w
    -> Pattern fs m ()

note w =
    self (Noting w ())



data Notatable r k
  where

    Notatable
        :: r
        -> (r -> k)
        -> Notatable r k



instance ( Binary r
         , Monoid r
         , Uses (Notatable r) gs m
         )
    => Binary (Attribute (Notatable r) gs m)
  where

    get =
        do
          r <- get
          let
            Notatable _ rk = writer

          return (Notatable r rk)

    put (Notatable r _) =
        put r



notator
    :: Uses (Notatable w) fs m
    => w
    -> (w -> w -> w)
    -> Attribute (Notatable w) fs m

notator w0 f =
    Notatable w0 $ \w' fs ->
        let
          Notatable w k =
              view fs

        in pure $ fs .=
               Notatable (f w w') k



writer
    :: ( Monoid w
       , Uses (Notatable w) fs m
       )
    => Attribute (Notatable w) fs m

writer =
    notator mempty (<>)



instance Witnessing (Notatable r) (Noting r)
  where

    witness use (Notatable _ rk) (Noting r k) =
        witness use rk (r,k)



noted
    :: Uses (Notatable w) fs m
    => Object fs m -> w

noted fs =
    let
      Notatable w _ =
          view fs

    in
      w



{-# INLINE note #-}
{-# INLINE notator #-}
{-# INLINE writer #-}
{-# INLINE noted #-}