{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Get.Lexicon where



import Ef.Core.Narrative
import Ef.Core.Object


data Lexicon k
  where

    Reset
        :: k
        -> Lexicon k

    Reify
        :: k
        -> Lexicon k

    Get
        :: (    Object gs m
             -> k
           )
        -> Lexicon k



type Gets lexicon environment =
    Knows Lexicon lexicon environment
