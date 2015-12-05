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
        :: Object attrs parent
        -> k
        -> Setting k



become
    :: ( (Attrs attrs) `Witnessing` (Symbol scope)
       , Is Setting scope parent
       )
    => Object attrs parent
    -> Pattern scope parent ()

become newSelf =
    self (Set newSelf ())



data Settable k
  where

    Setter
        :: (   Object attrs parent
            -> k
           )
        -> Settable k



instance Uses Settable attrs parent
    => Binary (Attribute Settable attrs parent)
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
    :: Uses Settable attrs parent
    => Attribute Settable attrs parent

setter =
    Setter (const . pure)
