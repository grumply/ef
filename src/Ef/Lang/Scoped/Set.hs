{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Set where

import Ef.Core
import Unsafe.Coerce

data Setting k
  where

    Set
        :: Object gs m
        -> k
        -> Setting k


data Settable k
  where

    Setter
        :: (   Object gs m
            -> k
           )
        -> Settable k

instance Witnessing Settable Setting
  where

    witness use (Setter ok) (Set o k) =
        let
          obj =
              unsafeCoerce o

        in
          use (ok obj) k

setter
    :: Uses Settable gs m
    => Attribute Settable gs m

setter =
    Setter (const . pure)
