{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Scoped.Fiber.Operation
    ( Operation(..)
    , Ops(..)
    , query
    , module Ef.Lang.Scoped.Fiber.Status
    ) where



import Ef.Core.Narrative
import Ef.Lang.Scoped.Fiber.Status
import Ef.Lang.IO

import Data.IORef



data Operation status result
  where

    Operation
        :: IORef (Status status result)
        -> Operation status result



data Ops scope parent status result =
    Ops
        {
          notify
              :: status
              -> Narrative scope parent ()

        , supplement
              :: (    Maybe status
                   -> Maybe status
                 )
              -> Narrative scope parent ()
        }




query
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Narrative scope parent (Status status result)

query (Operation op) =
    io (readIORef op)
