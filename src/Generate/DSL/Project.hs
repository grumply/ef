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

import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Exit

import qualified Language.Haskell.TH as TH

import qualified Distribution.ModuleName as Dist

import Prelude hiding (log)

import Debug.Trace

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
  MopState{..} <- get
  MopContext{..} <- ask
  let m@(Module _ _ _ _ _ _ decls) = originalModule

  (symbolsName,at) <- findProject m

  let ty = getTypeByName symbolsName decls

  when (null ty) $
      log Error $ "Generate.DSL.Project.createProjection: Could not find " ++ symbolsName

  when (length ty > 1) $
      log Error $ "Generate.DSL.Project.createProjection: Found multiple types for " ++ symbolsName

  symbolNames <- breakSymbolSet (head ty)

  let md = takeDirectory cabalFile </> mopDirectory

  let symbolsModuleName      = Dist.fromString ("Symbols." ++ symbolsName)
      absSymbolsModule       = md </> Dist.toFilePath symbolsModuleName <.> "hs"

      tapeModuleName         = Dist.fromString ("Tape." ++ symbolsName)
      absTapeModule          = md </> Dist.toFilePath tapeModuleName <.> "hs"

      instructionsModuleName = Dist.fromString ("Instructions." ++ symbolsName)
      absInstructionsModule  = md </> Dist.toFilePath instructionsModuleName <.> "hs"

      pairingsModuleName     = Dist.fromString ("Pairings." ++ symbolsName)
      absPairingsModule      = md </> Dist.toFilePath pairingsModuleName <.> "hs"

      computerModuleName     = Dist.fromString ("Computer." ++ symbolsName)
      absComputerModule      = md </> Dist.toFilePath computerModuleName <.> "hs"

  guaranteeSourceDir mopDirectory

  mapM_ (\x -> createEmptyModule x mopDirectory >>= writeModule)
    [ symbolsModuleName
    , tapeModuleName
    , instructionsModuleName
    , computerModuleName
    , pairingsModuleName
    ]

  mapM_ (uncurry guaranteeImport)
    [ (mopTape                       ,absTapeModule)
    , (symbolsModule symbolsName     ,absTapeModule)
    , (prelude                       ,absTapeModule)

    , (mopComputer                   ,absComputerModule)
    , (instructionsModule symbolsName,absComputerModule)
    , (prelude                       ,absComputerModule)

    , (mopInstructions               ,absInstructionsModule)
    , (prelude                       ,absInstructionsModule)

    , (mopSymbols                    ,absSymbolsModule)
    , (prelude                       ,absSymbolsModule)

    , (mopPairings                   ,absPairingsModule)
    , (instructionsModule symbolsName,absPairingsModule)
    , (symbolsModule symbolsName     ,absPairingsModule)
    , (prelude                       ,absPairingsModule)
    ]

  tapesl         <- findEnd absTapeModule
  computersl     <- findEnd absComputerModule
  instructionssl <- findEnd absInstructionsModule
  symbolssl      <- findEnd absSymbolsModule
  pairingssl     <- findEnd absPairingsModule

  symbols_splices      <- transferSymbols                       symbolNames
  tape_splices         <- synthesizeTape         tapesl         symbolNames
  computer_splices     <- synthesizeComputer     computersl     symbolNames
  instructions_splices <- synthesizeInstructions instructionssl symbolNames
  pairings_splices     <- synthesizePairings     pairingssl     symbolNames

  mapM_ addExposedModule
    [ symbolsModuleName
    , tapeModuleName
    , instructionsModuleName
    , computerModuleName
    , pairingsModuleName
    ]

  mapM_ (\(sl,spls) -> mapM_ (spliceAtEnd sl) spls)
    [ (tapesl        ,tape_splices         )
    , (computersl    ,computer_splices     )
    , (instructionssl,instructions_splices )
    , (pairingssl    ,pairings_splices     )
    ]


  mapM_ (spliceTHAtEndWith symbolssl convertExistentialFunctor) symbols_splices

  mapM_ (flip transitionExtension absTapeModule)
    [ noMonomorphismRestrictionPragma
    , deriveFunctorPragma
    , flexibleContextsPragma
    , typeOperatorsPragma
    , existentialQuantificationPragma
    , kindSignaturesPragma
    , standaloneDerivingPragma
    ]

  modifyVersion incrementMinor

  void (unsplice (srcLine at) 1 (srcFilename at))

replaceStr :: String -> String -> String -> String
replaceStr old@(length -> n) new = go
  where
    go [] = []
    go (splitAt n -> ((==) old -> True,rest)) = new ++ go rest
    go str = head str : go (tail str)

getDataHead :: String -> String
getDataHead str =
  let dh = takeWhile (\x -> x /= '\n' && x /= '=') str
      is = findIndices (=='(') dh
  in case is of
       [] -> error "Generate.DSL.Project.getDataHead: Unexpectedly small Data head."
       xs -> fst (splitAt (pred (last xs)) dh)

convertExistentialFunctor :: String -> String
convertExistentialFunctor str =
  if "forall . " `isInfixOf` str
  then let stripped = replaceStr "forall . " "" str
           dataHead = getDataHead stripped
           funcHead = drop 5 -- remove 'data '
                    $ exhaust ( replaceStr " ->)" ")"
                              . replaceStr " *)" ")"
                              . replaceStr ":: *" ""
                              ) dataHead
       in replaceStr "\n    deriving (Functor)"
                     ("\nderiving instance Functor (" ++ funcHead ++ ")")
                     stripped
  else str

breakSymbolSet :: Decl -> Mop [Name]
breakSymbolSet ~(TypeType ty) = go [] ty
  where
    go acc     (TA x _)                = return $ reverse (getTyCon x:acc)
    go acc     (TC nm)                 = return $ reverse (nm:acc)
    go acc (TI (TA x _) (Sym ":+:") r) = go ((getTyCon x):acc) r
    go acc (TI (TC nm ) (Sym ":+:") r) = go (nm:acc) r
    go (reverse -> acc) ty'            = do
      log Error $ "Generate.DSL.Project.breakSymbolSetType:"
                   ++ "\n\tBad type: "        ++ prettyPrint ty'
                   ++ "\n\tContinuing with: " ++ unlines (map prettyPrint acc)
      return acc

    getTyCon (TC nm)  = nm
    getTyCon (TA x _) = getTyCon x

synthesizeTape :: SrcLoc -> [Name] -> Mop [Decl]
synthesizeTape at = fmap concat <$> mapM (createTape at <=< typeInfo <=< reify)
                  . map convertNm

createTape :: SrcLoc -> THInfo -> Mop [Decl]
createTape sl THInfo{..} = fmap concat <$>
    flip mapM (zip [0..] infoTerms) $ \(n,(con,map snd -> ts)) -> do
      let vs = safeInit (deduplicateNames $ map smartName ts)
          buildSymbol = foldr (\nm cont st -> cont (App (st (Var (UnQual nm)))))
                              (\res -> if null ts
                                       then case res undefined of App l _ -> l
                                       else if isHigherKinded (last ts)
                                            then res (Var (UnQual (Ident "id")))
                                            else res (Var (Special UnitCon))
                              )
                              vs
                              (App (Con (UnQual (Ident (TH.nameBase con)))))
      return $ [FunBind
                  [Match
                     sl
                     (Ident (uncapitalize (TH.nameBase con)))
                     (map PVar vs)
                     Nothing
                     (UnGuardedRhs (App (Var (UnQual (Ident "liftF")))
                                        (Paren (App (Var (UnQual (Ident "inj")))
                                                    (Paren buildSymbol)
                                               )
                                        )
                                   )
                     )
                     (BDecls [])
                  ]
               ]

synthesizePairings :: SrcLoc -> [Name] -> Mop [Decl]
synthesizePairings at =
  fmap concat <$> mapM (createPairings at <=< typeInfo <=< reify)
  . map convertNm

createPairings :: SrcLoc -> THInfo -> Mop [Decl]
createPairings at THInfo{..} = return []

synthesizeInstructions :: SrcLoc -> [Name] -> Mop [Decl]
synthesizeInstructions at =
  fmap concat <$> mapM (createInstructions at <=< typeInfo <=< reify)
  . map convertNm

createInstructions :: SrcLoc -> THInfo -> Mop [Decl]
createInstructions sl THInfo{..} = return []

-- Most symbol sets will be written in a development module and
-- automatically transferred to a properly namespace-d module
-- upon projection. This function will strip unnecessary type
-- namespace qualifications as well as add a functor deriving
-- instance if one does not exist.
transferSymbols :: [Name] -> Mop [TH.Dec]
transferSymbols nms = do
  infos <- mapM reify (map convertNm nms)
  return $ map addFunctorDeriving [ d | (TH.TyConI d) <- infos ]
  where
    addFunctorDeriving (TH.DataD cxt nm tvs cons ds) =
      TH.DataD
        (map simplifyType cxt)
        (baseName nm)
        tvs
        (map simplifyConName cons)
        (nub (TH.mkName "Functor":ds))
    addFunctorDeriving (TH.NewtypeD cxt nm tvs con ds) =
      TH.NewtypeD
        (map simplifyType cxt)
        (baseName nm)
        tvs
        (simplifyConName con)
        (nub (TH.mkName "Functor":ds))
    addFunctorDeriving dec =
      error $ "Generate.DSL.Project.transferSymbols.addFunctorDeriving: not a data type declaration: " ++ show dec

    simplifyConName (TH.NormalC nm ts)       =
      TH.NormalC (baseName nm) (map (second simplifyType) ts)
    simplifyConName (TH.RecC nm vsts)        =
      TH.RecC (baseName nm) (map (\(x,y,z) -> (baseName x,y,simplifyType z)) vsts)
    simplifyConName (TH.InfixC stl nm str)   =
      TH.InfixC (second simplifyType stl) (baseName nm) (second simplifyType str)
    simplifyConName (TH.ForallC tvs cxt con) =
      TH.ForallC tvs (map simplifyType cxt) (simplifyConName con)

    simplifyType (TH.VarT nm)      = TH.VarT nm
    simplifyType (TH.ConT nm)      = TH.ConT (baseName nm)
    simplifyType (TH.PromotedT nm) = TH.PromotedT (baseName nm)
    simplifyType (TH.AppT l r)     = TH.AppT (simplifyType l) (simplifyType r)
    simplifyType (TH.ForallT tvs cxt ty) = TH.ForallT tvs (map simplifyType cxt) (simplifyType ty)
    simplifyType x = x

synthesizeComputer :: SrcLoc -> [Name] -> Mop [Decl]
synthesizeComputer at =
  fmap concat <$> mapM (createComputer at <=< typeInfo <=< reify)
  . map convertNm

createComputer :: SrcLoc -> THInfo -> Mop [Decl]
createComputer sl THInfo{..} = return []
