{-# LANGUAGE GADTs #-}
module Signaled where

import qualified Ef.Event as React

import Event
import Queue
import Unsafe.Coerce

data BufferedSignal
    where
        BufferedSignal
            :: React.Signal self super SomeEvent
            -> Queue SomeEvent
            -> BufferedSignal

instance Eq BufferedSignal where
    (==) (BufferedSignal _ q) (BufferedSignal _ q') = q == (unsafeCoerce q')


data Signaled where
    Signaled :: Queue ([SomeEvent],React.Signal self super SomeEvent)
             -> Signaled
