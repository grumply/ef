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
    :: w
    -> Method (Noting w) lexicon environment ()

note w =
    say (Noting w ())



data Notatable r k
  where

    Notatable
        :: r
        -> (r -> k)
        -> Notatable r k



instance ( Binary r
         , Monoid r
         , Admits' (Notatable r) attrs (IndexOf (Notatable r) attrs)
         , Monad environment
         )
    => Binary (Attribute (Notatable r) attrs environment)
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
    :: ( Admits' (Notatable w) attrs (IndexOf (Notatable w) attrs)
       , Monad environment
       )
    => w
    -> (w -> w -> w)
    -> Attribute (Notatable w) attrs environment

notator w0 f =
    Notatable w0 $ \w' fs ->
        let
          Notatable w k =
              view fs

        in pure $ fs .=
               Notatable (f w w') k



writer
    :: ( Monoid w
       , Admits' (Notatable w) attrs (IndexOf (Notatable w) attrs)
       , Monad environment
       )
    => Attribute (Notatable w) attrs environment

writer =
    notator mempty (<>)



instance Inflection (Notatable r) (Noting r)
  where

    inflect use (Notatable _ rk) (Noting r k) =
        inflect use rk (r,k)



noted
    :: ( Admits' (Notatable w) attrs (IndexOf (Notatable w) attrs)
       , Monad environment
       )
    => Object attrs environment -> w

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
