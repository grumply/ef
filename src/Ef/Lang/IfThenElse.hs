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



data ITEing k
  where

    ITE
        :: (IORef (Pattern fs m a))
        -> (IORef (Pattern fs m a))
        -> k
        -> ITEing k



data ITEable k
  where

    ITEable
        :: k
        -> ITEable k



instance Uses ITEable gs m
    => Binary (Attribute ITEable gs m)
  where

    get =
        return ifThenElser


    put _ =
        pure ()



ifThenElse
    :: Is ITEing fs m
    => Bool
    -> Pattern fs m a
    -> Pattern fs m a
    -> Pattern fs m a

ifThenElse i t e =
    let
      t' =
          unsafePerformIO $ newIORef t

      e' =
          unsafePerformIO $ newIORef e

    in
      join $ self $ ITE t' e' $
          case i of

              True ->
                  unsafePerformIO $ readIORef t'

              False ->
                  unsafePerformIO $ readIORef e'



ifThenElser
    :: Uses ITEable fs m
    => Attribute ITEable fs m

ifThenElser =
    ITEable return



instance ITEable `Witnessing` ITEing
  where

    witness use (ITEable k) (ITE _ _ k') =
        use k k'
