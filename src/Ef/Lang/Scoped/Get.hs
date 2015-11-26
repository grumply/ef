{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Get where



import Ef.Core
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
    :: ( (Attrs gs) `Witnessing` (Symbol fs)
       , Is Getting fs m
       )
    => Pattern fs m (Object gs m)
introspect =
    do
      self (Reify ())
      self (Get id)



data Gettable k
  where

    Getter
        :: (Object gs m,k)
        -> k
        -> Gettable k



instance Witnessing Gettable Getting
  where

    witness use (Getter _ k) (Reify k') =
        use k k'

    witness use (Getter (o,k) _) (Get ok) =
        use k (ok (unsafeCoerce o))



getter
    :: Uses Gettable gs m
    => Attribute Gettable gs m

getter =
    Getter (undefined,reifier) pure
  where

    reifier fs =
        case view fs of

            Getter (_,reifies) gets ->
                pure $ fs .=
                     Getter (fs,reifies) gets
