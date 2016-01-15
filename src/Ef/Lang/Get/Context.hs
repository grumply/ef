{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Lang.Get.Context
    ( Gets(..)
    , gets
    ) where



import Ef.Core.Object
import Ef.Lang.Get.Attribute

import qualified Data.Binary as B



instance ( Has Gets contexts environment
         , B.Binary (Object contexts environment)
         )
    => B.Binary (Gets (Morphism contexts environment))
  where

    get =
        pure gets


    put _ =
        pure ()



gets
    :: Use Gets contexts environment

gets =
    Gets (undefined,reifier) resetter pure
  where

    resetter fs =
        case view fs of

            Gets (_,reifies) reset gets ->
                pure $ fs .=
                    Gets (undefined,reifies) reset gets

    reifier fs =
        case view fs of

            Gets _ reset gets ->
                pure $ fs .=
                     Gets (fs,reifier) reset gets

{-# INLINE gets #-}
