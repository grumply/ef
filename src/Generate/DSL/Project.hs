{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Generate.DSL.Project where

import Generate.Cabal
import Generate.DSL.Helpers
import Generate.Monad
import Generate.Representation
import Generate.Splice
import Generate.Splice.Import
import Generate.Splice.Pragma
import Generate.Utils

import Data.List
import Data.Maybe
import System.Exit

import qualified Language.Haskell.TH as TH

import Prelude hiding (log)

project :: String -> TH.Q [TH.Dec]
project str = return [expandProjectSplice str]

expandProjectSplice :: String -> TH.Dec
expandProjectSplice str =
  TH.FunD
    (TH.mkName "project")
    [TH.Clause
       []
       (TH.NormalB
          (TH.VarE (TH.mkName str))
       )
       []
    ]

hasProject :: [TH.Dec] -> Bool
hasProject [] = False
hasProject (TH.FunD (TH.nameBase -> "project") _:_) = True
hasProject (_:xs) = hasProject xs

findProject :: Module -> Mop (String,SrcLoc)
findProject (Module _ _ _ _ _ _ decls) =
  case mapMaybe getProjectSplice decls of
    (x:_) -> return x
    _ -> do log error "No project splice found in module decls."
            io exitFailure
  where
    getProjectSplice (SpliceDecl sl@(SrcLoc _ l _) e) =
      case e of
        App (Var (UnQual (Ident "mop")))
            (Paren (App (Var (UnQual (Ident "project")))
                        (Lit (String x))
                   )
            )         -> Just (x,sl)
        _             -> Nothing
    getProjectSplice _ = Nothing

projectSymbols :: Mop [TH.Dec]
projectSymbols = do
  createProjection
  return []

createProjection :: Mop ()
createProjection = do
  log Debug "Creating projection"
  MopContext{..} <- ask
  let f = TH.loc_filename location
      m@(Module _ _ _ _ _ imprts decls) = originalModule

  (symbolsName,at) <- findProject m

  let ty = getTypeByName symbolsName decls

  when (null ty) $
      log Error $ "Generate.DSL.Project.createProjection: Could not find " ++ symbolsName

  when (length ty > 1) $
      log Error $ "Generate.DSL.Project.createProjection: Found multiple types for " ++ symbolsName

  symbolNames <- breakSymbolSetDecl (head ty)

  symbols <- gatherTypes symbolNames decls imprts

  mapM (splice at <=< createSymbol) symbols

  return ()

breakSymbolSetDecl :: Decl -> Mop [Name]
breakSymbolSetDecl ~(TypeDecl _ _ _ ty) = breakSymbolSetType ty

breakSymbolSetType :: Type -> Mop [Name]
breakSymbolSetType = return . go []
  where
    go acc (TyApp x _) = reverse (getTyCon x:acc)
    go acc (TyInfix (TyApp x _) (UnQual (Symbol ":+:")) r) = go ((getTyCon x):acc) r
    go acc (TyInfix (TyCon (UnQual nm)) (UnQual (Symbol ":+:")) r) = go (nm:acc) r
    getTyCon (TyCon (UnQual nm)) = nm
    getTyCon (TyApp x _) = getTyCon x

gatherTypes :: [Name] -> [Decl] -> [ImportDecl] -> Mop [Decl]
gatherTypes symbols decls imprts = undefined

createSymbol :: Decl -> Mop Decl
createSymbol (DataDecl _ _ _ nm tyVars _ _) = undefined
