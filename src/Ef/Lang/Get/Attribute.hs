{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Ef.Lang.Get.Attribute where


import Ef.Core

import qualified Data.Binary as B


data Getter k
  where

    Getter
        :: (Object attrs parent,k)
        -> k
        -> k
        -> Getter k



instance ( Uses Getter attrs parent
         , B.Binary (Object attrs parent)
         )
    => B.Binary (Attribute Getter attrs parent)
  where

    get =
        return (getter :: Attribute Getter attrs parent)


    put _ =
        pure ()

getter
    :: Implementation Getter attrs parent

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

            Getter (_,reifies) reset gets ->
                pure $ fs .=
                     Getter (fs,reifies) reset gets


{-# INLINE getter #-}
