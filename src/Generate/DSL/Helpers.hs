{-# LANGUAGE ViewPatterns #-}
module Generate.DSL.Helpers (module Export,isHigherKinded,smartName,makeVar) where

import Generate.DSL.Helpers.TH as Export
import Generate.DSL.Helpers.HSE as Export
import Generate.Utils

import qualified Language.Haskell.TH as TH

import Language.Haskell.Exts

isHigherKinded :: TH.Type -> Bool
isHigherKinded (TH.AppT _ _) = True
isHigherKinded _ = False

smartName :: TH.Type -> Name
smartName = Ident . go []
  where
    go acc (TH.AppT l r) =
      let accl = go [] l
          accr = go [] r
      in acc ++ accl ++ accr
    go acc (TH.VarT nm) = acc ++ (TH.nameBase nm)
    go acc (TH.ConT nm) = acc ++ (uncapitalize (TH.nameBase nm))
    go acc _ = acc


makeVar :: TH.Name -> Pat
makeVar (TH.nameBase -> nm) = PVar (Ident nm)
