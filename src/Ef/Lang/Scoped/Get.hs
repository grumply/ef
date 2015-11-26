{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Get where

import Ef.Core
import Unsafe.Coerce
import Data.Typeable



-- | Symbol

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



-- | Symbol Modules

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
