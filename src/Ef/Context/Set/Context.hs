{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Context.Set.Context
    ( Sets(..)
    , sets
    ) where



import Ef.Core.Object
import Ef.Context.Set.Attribute

import qualified Data.Binary as B



instance ( Has Sets contexts environment
         , B.Binary (Object contexts environment)
         )
    => B.Binary (Sets (Morphism contexts environment))
    where

        get =
            pure sets

        put _ =
            pure ()



sets
    :: Use Sets contexts environment

sets =
    Sets (const . pure)
