{-# LANGUAGE GADTs #-}
module Ef.Context.Get.Lexicon
    ( Get(..)
    ) where



import Ef.Core.Narrative
import Ef.Core.Object


data Get k
    where

        Reset
            :: k
            -> Get k

        Reify
            :: k
            -> Get k

        Get
             :: (    Object gs m
                  -> k
                )
             -> Get k
