{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Get.Methods
    ( Get(..)
    , get
    ) where


import Ef.Object


data Get k where
    Get :: (Object methods super,k) -> k -> k -> Get k


get :: Use Get methods super
get =
    Get (undefined,reifier) resetter pure
  where

    resetter fs =
        case view fs of

            Get (_,reifies) reset gets ->
                pure $ fs .=
                    Get (undefined,reifies) reset gets

    reifier fs =
        case view fs of

            Get _ reset gets ->
                pure $ fs .=
                     Get (fs,reifier) reset gets

{-# INLINE get #-}
