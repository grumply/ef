{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Lang.Scoped.Fiber.Context
    ( Fibers(..)
    , fibers
    ) where



import Ef.Core.Object
import Ef.Lang.Scoped.Fiber.Attribute

import qualified Data.Binary as B



instance Has Fibers contexts environment
    => B.Binary (Fibers (Morphism contexts environment))
  where

    get =
        return fibers



    put _ =
        pure ()



fibers
    :: Use Fibers attrs parent

fibers =
    Fibers 0 $ \fs ->
        let
            Fibers i k =
                view fs

            i' =
                succ i

        in
            i' `seq` pure $ fs .=
                Fibers i' k
