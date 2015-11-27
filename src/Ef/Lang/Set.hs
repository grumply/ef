{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Set where



import Ef.Core

import Data.Binary
import Unsafe.Coerce



data Setting k
  where

    Set
        :: Object gs m
        -> k
        -> Setting k



become
    :: ( (Attrs gs) `Witnessing` (Symbol fs)
       , Is Setting fs m
       )
    => Object gs m
    -> Pattern fs m ()

become newSelf =
    self (Set newSelf ())



data Settable k
  where

    Setter
        :: (   Object gs m
            -> k
           )
        -> Settable k



instance Uses Settable gs m
    => Binary (Attribute Settable gs m)
  where

    get =
        pure setter



    put _ =
        pure ()



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
