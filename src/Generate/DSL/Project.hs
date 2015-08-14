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

  io (print "symbolNames")

  symbols <- gatherSymbols at symbolNames decls

  io (print "symbols")
  io (mapM_ print symbols)

  sequence_ (foldr ((>>>) . (:) . (splice at <=< createSymbol)) id symbols [])

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
  io (print (symbolCount,"symbolCount"))
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

{-

DataD
  []

  GHC.Base.Maybe

  [KindedTV a_1627397280 StarT]

  [NormalC GHC.Base.Nothing []
  ,NormalC GHC.Base.Just [(NotStrict,VarT a_1627397280)]
  ]

  []

DataD Cxt Name [TyVarBndr] [Con] [Name] => data Cxt x => T x = A x | B (T x) deriving (Z,W)

DataD
  []

  Other.E

  [KindedTV s_1627391301 StarT
  ,KindedTV k_1627391302 StarT
  ]

  [ForallC [] [AppT (ConT GHC.Classes.Eq) (VarT s_1627391301)]
              (NormalC Other.E
                 [(NotStrict,VarT s_1627391301),(NotStrict,VarT k_1627391302)]
              )
  ]

  []

Module
  (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 1 1)
  (ModuleName "Other")
  [LanguagePragma
     (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 1 1)
     [Ident "TypeFamilies"]
  ,LanguagePragma
     (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 2 1)
     [Ident "ExistentialQuantification"]
  ,LanguagePragma
     (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 3 1)
     [Ident "MultiParamTypeClasses"]
  ]
  Nothing
  Nothing
  []
  [ClassDecl
     (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 6 1)
     []
     (Ident "Multi")
     [KindedVar (Ident "x") (KindFn KindStar KindStar)
     ,UnkindedVar (Ident "y")
     ]
     []
     [ClsDecl
        (TypeSig
           (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 7 3)
           [Ident "multi"]
           (TyFun
              (TyVar (Ident "y"))
              (TyApp (TyVar (Ident "x")) (TyVar (Ident "y")))
           )
        )
     ]
  ,DataDecl
     (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 9 1)
     DataType
     []
     (Ident "E")
     [UnkindedVar (Ident "s")
     ,UnkindedVar (Ident "k")
     ]
     [QualConDecl
        (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 9 24)
        []
        [ParenA
           (ClassA
              (UnQual (Ident "Eq"))
              [TyVar (Ident "s")]
           )
        ]
        (ConDecl
           (Ident "E")
           [TyVar (Ident "s")
           ,TyVar (Ident "k")
           ]
        )
     ]
     []
  ,DataDecl
     (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 11 1)
     DataType
     []
     (Ident "F")
     [UnkindedVar (Ident "t")
     ,UnkindedVar (Ident "m")
     ,UnkindedVar (Ident "k")
     ]
     [QualConDecl
        (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 11 31)
        []
        [ParenA
           (ClassA
              (UnQual (Ident "Multi"))
              [TyVar (Ident "t")
              ,TyVar (Ident "m")
              ]
           )
        ]
        (ConDecl
           (Ident "F")
           [TyParen
              (TyApp
                 (TyVar (Ident "t"))
                 (TyVar (Ident "m"))
              )
           ,TyVar (Ident "k")
           ]
         )
     ]
     []
  ,DataDecl
     (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 13 1)
     DataType
     []
     (Ident "G")
     [UnkindedVar (Ident "r")
     ,UnkindedVar (Ident "s")
     ,UnkindedVar (Ident "k")
     ]
     [QualConDecl
        (SrcLoc "/media/sean/Server/Sean/mop-test/src/Other.hs" 13 31)
        []
        [ParenA
           (EqualP
              (TyVar (Ident "r"))
              (TyApp
                 (TyApp
                    (TyCon (UnQual (Ident "E")))
                    (TyVar (Ident "s"))
                 )
                 (TyVar (Ident "k"))
              )
           )
        ]
        (ConDecl
           (Ident "G")
           [TyVar (Ident "r")
           ,TyVar (Ident "s")
           ,TyVar (Ident "k")
           ]
        )
     ]
     []
  ]
-}

convertSymbolInfo :: SrcLoc -> TH.Info -> Mop (Maybe Decl)
convertSymbolInfo sl (TH.TyConI tci) = do
  io (print tci)
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
    go (TH.RecC    _ _   ) (sl,qcds) = undefined
    go (TH.InfixC  _ _ _ ) (sl,qcds) = undefined
    go (TH.ForallC _ _ _ ) (sl,qcds) = undefined

convertStrictType :: (TH.Strict,TH.Type) -> Type
convertStrictType (TH.IsStrict,ty)  = TyBang BangedTy   (convertType ty)
convertStrictType (TH.Unpacked,ty)  = TyBang UnpackedTy (convertType ty)
convertStrictType (TH.NotStrict,ty) =                    convertType ty

convertType :: TH.Type -> Type
convertType (TH.ForallT tyVarBndrs cxt ty)    = TyForall
  (if null tyVarBndrs then Nothing else Just (convertTVars tyVarBndrs))
  (map convertTypeToAssertion cxt)
  (convertType ty)
convertType (TH.AppT TH.ArrowT (TH.AppT l r)) = TyFun (convertType l) (convertType r)
convertType (TH.AppT TH.ListT ty)             = TyList (convertType ty)
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
convertType _                = error
  "Generate.DSL.Project.convertType: convertType does not yet handle promoted types."

convertTypeList :: TH.Type -> [Type]
convertTypeList (TH.AppT l r) = convertTypeList l ++ convertTypeList r
convertTypeList t = [convertType t]

convertTypeToAssertion :: TH.Type -> Asst
convertTypeToAssertion _ = undefined

createSymbol :: Decl -> Mop Decl
createSymbol d@(DataCons cs) =
  if length cs > 1
  then createClosedSymbol d
  else createOpenSymbol d

createClosedSymbol _ = undefined

createOpenSymbol _ = undefined
