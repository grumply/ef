{-# LANGUAGE GADTs #-}
module Ef.Lang.Scoped.Fiber.Status
    ( Status(..)
    ) where



import Control.Exception



data Status status result
  where

    Running
        :: Maybe status
        -> Status status result

    Failed
        :: SomeException
        -> Status status result

    Done
        :: result
        -> Status status result
