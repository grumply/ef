{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Ef.Fiber.Messages where


import Ef.Narrative
import Ef.IO

import Control.Exception
import Data.IORef
import Unsafe.Coerce


-- | Status represents the state of a thread of execution.
-- The status may be updated during
data Status status result
    = Running (Maybe status)
    | Failed SomeException
    | Done result

data Operation status result =
    Operation (IORef (Status status result))


data Ops scope parent status result = Ops
    { notify :: status -> Narrative scope parent ()
    , supplement :: (Maybe status -> Maybe status) -> Narrative scope parent ()
    }

query
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Narrative scope parent (Status status result)

query (Operation op) =
    io (readIORef op)

data Fiber k where
    Fork :: Int -> Operation status result -> Narrative scope parent result -> Fiber k
    Yield :: Int -> Fiber k
    Focus :: Int -> Narrative scope parent result -> Fiber k
    FreshScope :: (Int -> k) -> Fiber k


data Threader self super =
    Threader
        {
          fork :: forall status result.
                 (   Ops self super status result
                  -> Narrative self super result
                 )
              -> Narrative self super (Operation status result)

        , await
              :: forall status result.
                 Operation status result
              -> Narrative self super (Status status result)

        , focus
              :: forall focusResult.
                 Narrative self super focusResult
              -> Narrative self super focusResult

        , yield
              :: Narrative self super ()

        , chunk
              :: forall chunkResult.
                 Int
              -> Narrative self super chunkResult
              -> Narrative self super chunkResult
        }
