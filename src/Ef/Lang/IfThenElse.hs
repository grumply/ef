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
        :: (IORef (Pattern scope parent result))
        -> (IORef (Pattern scope parent result))
        -> k
        -> ITEing k



data ITEable k
  where

    ITEable
        :: k
        -> ITEable k



instance Uses ITEable attrs parent
    => Binary (Attribute ITEable attrs parent)
  where

    get =
        return ifThenElser


    put _ =
        pure ()



ifThenElse
    :: Is ITEing scope parent
    => Bool
    -> Pattern scope parent result
    -> Pattern scope parent result
    -> Pattern scope parent result

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
          self (ITE t' e' continue)

    in
      join conditional



ifThenElser
    :: Uses ITEable scope parent
    => Attribute ITEable scope parent

ifThenElser =
    ITEable return



instance ITEable `Witnessing` ITEing
  where

    witness use (ITEable k) (ITE _ _ k') =
        use k k'
