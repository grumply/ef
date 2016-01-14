{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Lang.Get.Context
    ( Attribute(..)
    , getter
    ) where



import Ef.Core.Object
import Ef.Lang.Get.Attribute

import qualified Data.Binary as B



instance ( Gets contexts environment
         , B.Binary (Object contexts environment)
         )
    => B.Binary (Attribute (Morphism contexts environment))
  where

    get =
        pure getter


    put _ =
        pure ()



getter
    :: Use Attribute contexts environment

getter =
    Getter (undefined,reifier) resetter pure
  where

    resetter fs =
        case view fs of

            Getter (_,reifies) reset gets ->
                pure $ fs .=
                    Getter (undefined,reifies) reset gets

    reifier fs =
        case view fs of

            Getter _ reset gets ->
                pure $ fs .=
                     Getter (fs,reifier) reset gets

{-# INLINE getter #-}
