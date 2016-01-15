{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Ef.Lang.Vary.Lexicon
    ( Vary(..)
    ) where



import Ef.Core.Narrative



data Vary st k
    where

        Modify
            :: (st -> st)
            -> (st -> k)
            -> Vary st k
