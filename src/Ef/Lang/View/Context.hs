{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ef.Lang.View.Context
    ( Views(..)
    ) where


import Ef.Core.Object
import Ef.Lang.View.Attribute

import qualified Data.Binary as B



instance ( Has (Views r) contexts environment
         , B.Binary r
         )
    => B.Binary (Views r (Morphism contexts environment))
  where

    get =
        do
          r <- B.get
          return (views r)

    put (Views r _) =
        B.put r



views
    :: r
    -> Use (Views r) scope environment

views r =
    Views r pure
