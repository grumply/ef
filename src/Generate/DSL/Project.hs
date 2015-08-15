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

import Control.Arrow

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
        App (VUI "mop") (PA (VUI "project") (Str x)) -> Just (x,sl)
        _                                            -> Nothing
    getProjectSplice _                                = Nothing

projectSymbols :: Mop [TH.Dec]
projectSymbols = do
  createProjection
  return []

createProjection :: Mop ()
createProjection = do
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

  symbols <- gatherSymbols at symbolNames decls

  sequence_ (foldr ((>>>) . (:) . (mapM (splice at) <=< createSymbol)) id symbols [])

  io (print "sequence_")

  return ()

breakSymbolSetDecl :: Decl -> Mop [Name]
breakSymbolSetDecl ~(TypeType ty) = breakSymbolSetType ty

breakSymbolSetType :: Type -> Mop [Name]
breakSymbolSetType ty = go [] ty
  where
    go acc     (TA x _)                = return $ reverse (getTyCon x:acc)
    go acc     (TC nm)                 = return $ reverse (nm:acc)
    go acc (TI (TA x _) (Sym ":+:") r) = go ((getTyCon x):acc) r
    go acc (TI (TC nm ) (Sym ":+:") r) = go (nm:acc) r
    go (reverse -> acc) ty             = do
      log Error $ "Generate.DSL.Project.breakSymbolSetType:"
                   ++ "\n\tBad type: "        ++ prettyPrint ty
                   ++ "\n\tContinuing with: " ++ unlines (map prettyPrint acc)
      return acc

    getTyCon (TC nm)  = nm
    getTyCon (TA x _) = getTyCon x

gatherSymbols :: SrcLoc -> [Name] -> [Decl] -> Mop [Decl]
gatherSymbols at symbols@(length -> symbolCount) decls = do
  let localSymbols = [ (d,nm) | d@(DataName nm) <- decls
                              , nm `elem` symbols
                              ]
  if length localSymbols == symbolCount
  then return (map fst localSymbols)
  else do
    nonlocals <- fmap catMaybes $ mapM (convertSymbolInfo at <=< reifyHSE)
                                       (symbols \\ map snd localSymbols)
    return (map fst localSymbols ++ nonlocals)

reifyHSE :: Name -> Mop TH.Info
reifyHSE (Ident str)  = liftTH . TH.reify . TH.mkName $ str
reifyHSE (Symbol str) = liftTH . TH.reify . TH.mkName $ str

convertSymbolInfo :: SrcLoc -> TH.Info -> Mop (Maybe Decl)
convertSymbolInfo sl (TH.TyConI tci) = do
  case tci of
    TH.DataD _ nm tvars cons _ -> do
      return $ Just $
        DataDecl
          sl
          DataType
          []
          (convertName nm)
          (convertTVars tvars)
          (convertConstructors sl cons)
          []
    _ -> do
      log Error ("Generate.DSL.Project.convertSymbolInfo: got non-DataD in TyConI: " ++ show tci)
      return Nothing
convertSymbolInfo _ i = do
  io $ putStrLn $ "Generate.DSL.Project.convertSymbolInfo: expecting TH.TyConI; got: " ++ show i
  return Nothing

convertName :: TH.Name -> Name
convertName = Ident . TH.nameBase

convertNameToQName :: TH.Name -> QName
convertNameToQName = UnQual . convertName

convertTVars :: [TH.TyVarBndr] -> [TyVarBind]
convertTVars = map convertTyVarBndr
  where
    convertTyVarBndr (TH.PlainTV nm) = UnkindedVar (convertName nm)
    convertTyVarBndr (TH.KindedTV nm k) = KindedVar (convertName nm) (convertKind k)
      where
        convertKind :: TH.Kind -> Kind
        convertKind (TH.AppT TH.StarT r) = KindApp KindStar (convertKind r)
        convertKind TH.StarT = KindStar
        convertKind _ = error $
          "Generate.DSL.Project.convertTVars: \
          \Kind not understood (only handles (* [-> *])): "
            ++ show k

convertConstructors :: SrcLoc -> [TH.Con] -> [QualConDecl]
convertConstructors sl0 = reverse . snd . foldr go (lineAfterIndented sl0,[])
  where
    go (TH.NormalC nm sts) (sl,qcds) =
      let sl' = lineAfterIndented sl
          qcd = QualConDecl sl' [] []
                  (ConDecl (convertName nm)
                           (map convertStrictType sts)
                  )
      in (sl',qcd:qcds)
    go (TH.RecC    _ _   ) (sl,qcds) = error
      "Generate.DSL.Project.convertConstructors: convertConstructors got RecC."
    go (TH.InfixC  _ _ _ ) (sl,qcds) = error
      "Generate.DSL.Project.convertConstructors: convertConstructors got InfixC."
    go (TH.ForallC _ _ _ ) (sl,qcds) = error
      "Generate.DSL.Project.convertConstructors: convertConstructors got ForallC."

convertStrictType :: (TH.Strict,TH.Type) -> Type
convertStrictType (TH.IsStrict,ty)  = TyBang BangedTy   (convertType ty)
convertStrictType (TH.Unpacked,ty)  = TyBang UnpackedTy (convertType ty)
convertStrictType (TH.NotStrict,ty) =                    convertType ty

convertType :: TH.Type -> Type
convertType (TH.ForallT tyVarBndrs cxt ty)    = TyForall
  (if null tyVarBndrs then Nothing else Just (convertTVars tyVarBndrs))
  (map convertTypeToAssertion cxt)
  (convertType ty)
convertType (TH.AppT tyl tyr)                 = TyApp (convertType tyl) (convertType tyr)
convertType (TH.VarT nm)                      = TyVar (convertName nm)
convertType (TH.ConT nm)                      = TyCon (convertNameToQName nm)

convertType (TH.ListT)       = error
  "Generate.DSL.Project.convertType: convertType got an unexpected TH.ListT value."
convertType (TH.ConstraintT) = error
  "Generate.DSL.Project.convertType: convertType does not yet handle constraint types."
convertType (TH.EqualityT)   = error
  "Generate.DSL.Project.convertType: convertType does not yet handle equality constraints."
convertType (TH.SigT ty k)   = error
  "Generate.DSL.Project.convertType: convertType does not yet handle signature conversion."
convertType (TH.StarT)       = error
  "Generate.DSL.Project.convertType: convertType got an unexpected TH.StarT value."
-- convertType _                = error
--   "Generate.DSL.Project.convertType: convertType does not yet handle promoted types."

convertType ty = error ("Generate.DSL.Project.convertType: " ++ show ty)

convertTypeList :: TH.Type -> [Type]
convertTypeList (TH.AppT l r) = convertTypeList l ++ convertTypeList r
convertTypeList t = [convertType t]

convertTypeToAssertion :: TH.Type -> Asst
convertTypeToAssertion _ = error "Generate.DSL.Project.convertTypeToAssertion"

createSymbol :: Decl -> Mop [Decl]
createSymbol d@(DataCons cs) = do
  io (print d)
  if length cs > 1
  then createClosedSymbol d
  else createOpenSymbol d

{-
DataDecl
  (SrcLoc "src/Main.hs" 23 1)
  DataType
  []
  (Ident "Maybe")
  [KindedVar (Ident "a") KindStar]
  [QualConDecl
    (SrcLoc "src/Main.hs" 25 9)
    []
    []
    (ConDecl (Ident "Just") [TyVar (Ident "a")])
  ,QualConDecl
    (SrcLoc "src/Main.hs" 26 13)
    []
    []
    (ConDecl (Ident "Nothing") [])
  ]
  []
-}

createClosedSymbol (DataDecl sl DataType _ nm tyvars cons _) = do
  mapM createClosedSymbolsFromCon cons

createOpenSymbol _ = undefined
