{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Get where



import Ef.Core

import Data.Binary
import Unsafe.Coerce



data Getting k
  where

    Reify
        :: k
        -> Getting k

    Get
        :: (    Object gs m
             -> k
           )
        -> Getting k



introspect
    :: ( (Attrs attrs) `Witnessing` (Symbol scope)
       , Is Getting scope parent
       )
    => Pattern scope parent (Object attrs parent)

introspect =
    do
      self (Reify ())
      self (Get id)



data Gettable k
  where

    Getter
        :: (Object attrs parent,k)
        -> k
        -> Gettable k



instance ( Uses Gettable attrs parent
         , Binary (Object attrs parent)
         )
    => Binary (Attribute Gettable attrs parent)
  where

    get =
        return (getter :: Attribute Gettable attrs parent)


    put _ =
        pure ()



instance Witnessing Gettable Getting
  where

    witness use (Getter _ k) (Reify k') =
        use k k'

    witness use (Getter (o,k) _) (Get ok) =
        use k (ok (unsafeCoerce o))



getter
    :: Uses Gettable attrs parent
    => Attribute Gettable attrs parent

getter =
    Getter (undefined,reifier) pure
  where

    reifier fs =
        case view fs of

            Getter (_,reifies) gets ->
                pure $ fs .=
                     Getter (fs,reifies) gets


{-# INLINE getter #-}
{-# INLINE introspect #-}
