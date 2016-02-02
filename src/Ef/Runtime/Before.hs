{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
{- | (Ef) (O)bject (R)untime (E)nvironment; manager for an object environment that interfaces with GHCi.
     Supports reloading components, extending objects, and other live-develop functionality.
-}

module Ef.Runtime.Before where



import Ef.Core.Narrative
import Ef.Core.Object
import Ef.Core

import Ef.Lang.IO

import Control.Concurrent
import Control.Monad

import Data.IORef
import Data.Maybe

import System.Directory
import System.IO
import System.Timeout


main =
    do
        system <- initialize
        run system



--------------------------------------------------------------------------------
-- Configuration

data Configuration k



initializeConfiguration =
    do
        undefined



--------------------------------------------------------------------------------
-- State

data State k



initializeState =
    do
        undefined



--------------------------------------------------------------------------------
-- System

type System = '[Configuration,State]



buildSystem
    :: Use Configuration System IO
    -> Use State System IO
    -> Object System IO
    
buildSystem configuration state =
    Object $ configuration *:* state *:* Empty



--------------------------------------------------------------------------------
-- Initialization

initialize =
    do
        configuration <- initializeConfiguration
        state <- initializeState configuration
        liftM2 buildSystem configuration state



--------------------------------------------------------------------------------
-- Run

run =
    do
        undefined
