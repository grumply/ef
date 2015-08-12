{-# LANGUAGE ViewPatterns #-}
module Generate.DSL.Helpers where

import Generate.DSL
import Generate.Monad

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Prelude hiding (log)
