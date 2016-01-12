{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.IfThenElse where



import Ef.Core

import Control.Monad
import Data.Binary
import Data.IORef
import System.IO.Unsafe
import Prelude



data ITEing k
  where

    ITE
        :: (IORef (Narrative lexicon environment result))
        -> (IORef (Narrative lexicon environment result))
        -> k
        -> ITEing k



data ITEable k
  where

    ITEable
        :: k
        -> ITEable k



instance ( Admits' ITEable attrs (IndexOf ITEable attrs)
         , Monad environment
         )
    => Binary (Attribute ITEable attrs environment)
  where

    get =
        return ifThenElser


    put _ =
        pure ()



ifThenElse
    :: ( Allows' ITEing lexicon (IndexOf ITEing lexicon)
       , Monad environment
       )
    => Bool
    -> Narrative lexicon environment result
    -> Narrative lexicon environment result
    -> Narrative lexicon environment result

ifThenElse i t e =
    let
      t' =
          unsafePerformIO (newIORef t)

      e' =
          unsafePerformIO (newIORef e)

      continue =
          case i of

              True ->
                  unsafePerformIO (readIORef t')

              False ->
                  unsafePerformIO (readIORef e')

      conditional =
          say (ITE t' e' continue)

    in
      join conditional



ifThenElser
    :: Implementation ITEable lexicon environment

ifThenElser =
    ITEable return



instance ITEable `Inflection` ITEing
  where

    inflect use (ITEable k) (ITE _ _ k') =
        use k k'
