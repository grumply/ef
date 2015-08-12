{-# LANGUAGE ViewPatterns #-}
module Generate.DSL.Helpers where

import Generate.Monad

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Prelude hiding (log)

gatherTypeHeads :: [Decl] -> [(Name,[TyVarBind])]
gatherTypeHeads ds = [ (nm,tyvs) | (DataDecl _ _ _ nm tyvs _ _) <- ds ]

gatherTypeVars ds = [ tyvs | (DataDecl _ _ _ _ tyvs _ _) <- ds ]

gatherContexts ds = [ c | (DataDecl _ _ c _ _ _ _) <- ds ]

boundedDecls start stop decls =
  [ x | x@(DataDecl (SrcLoc _ l _) _ _ _ _ _ _) <- decls
      , l > start
      , l < stop
      ]

dataNamesAndVars decls =
  [ (n,vs) | DataDecl _ _ _ n vs _ _ <- decls ]
