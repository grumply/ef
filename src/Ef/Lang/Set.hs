{-# LANGUAGE RankNTypes #-}
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
        :: Object attrs environment
        -> k
        -> Setting k



become
    :: ( (Attrs attrs) `Inflection` (Lexeme lexicon)
       )
    => Object attrs environment
    -> Method Setting lexicon environment ()

become newSay =
    say (Set newSay ())



data Settable k
  where

    Setter
        :: (   Object attrs environment
            -> k
           )
        -> Settable k



instance ( Admits' Settable attrs (IndexOf Settable attrs)
         , Monad environment
         )
    => Binary (Attribute Settable attrs environment)
  where

    get =
        pure setter



    put _ =
        pure ()



instance Inflection Settable Setting
  where

    inflect use (Setter ok) (Set o k) =
        let
          obj =
              unsafeCoerce o

        in
          use (ok obj) k



setter
    :: Uses Settable attrs environment

setter =
    Setter (const . pure)
