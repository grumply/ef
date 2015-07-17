{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Synonyms where

import Control.Monad.Trans.Free

import Subsumption

import Data.List

pattern If fb <- (runFree -> Free fb)
pattern Result x <- (runFree -> Pure x)

pattern Case x <- (If (prj -> Just x))
pattern Done <- (Result _)



-- pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)
