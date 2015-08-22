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

-- Wyverns soon become dragons.

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
  instructions_splices <- synthesizeInstructions instructionssl symbolNames
  pairings_splices     <- synthesizePairings     pairingssl     symbolNames $ map ti instructions_splices
  computer_splices     <- synthesizeComputer     computersl     symbolNames

  mapM_ addExposedModule
    [ symbolsModuleName
    , tapeModuleName
    , instructionsModuleName
    , computerModuleName
    , pairingsModuleName
    ]

  mapM_ (\(sl,spls) -> mapM_ (spliceAtEnd sl) spls)
    [ (tapesl        ,tape_splices         )
    ]

  mapM_ (\(sl,ss) -> mapM_ (spliceTHAtEndWith sl convertExistentialFunctor) ss)
    [ (symbolssl,symbols_splices)
    ]

  mapM_ (\(sl,ss) -> mapM_ (spliceTHAtEndWith sl convertExistentialFunctorCo) ss)
    [(instructionssl,map simplify instructions_splices)
    ]

  mapM_ (\(sl,ss) -> mapM_ (spliceTHAtEnd sl) ss)
    [ (computersl    ,computer_splices)
    , (pairingssl    ,pairings_splices)
    ]

  mapM_ (flip transitionExtension absTapeModule)
    [ noMonomorphismRestrictionPragma
    , deriveFunctorPragma
    , flexibleContextsPragma
    , typeOperatorsPragma
    , existentialQuantificationPragma
    , kindSignaturesPragma
    ]

  addPragma "StandaloneDeriving" absInstructionsModule
  addPragma "RankNTypes" absInstructionsModule

  modifyVersion incrementMinor

  void (unsplice (srcLine at) 1 (srcFilename at))

transferSymbols :: [Name] -> Mop [TH.Dec]
transferSymbols nms = do
  infos <- mapM reify (map convertNm nms)
  return $ map (addFunctorDeriving . simplify) [ d | (TH.TyConI d) <- infos ]

simplify :: TH.Dec -> TH.Dec
simplify (TH.DataD cxt nm tvs cons ds) =
  TH.DataD (map simplifyType cxt) (baseName nm) tvs (map simplifyConName cons) ds
simplify (TH.NewtypeD cxt nm tvs con ds) =
  TH.NewtypeD (map simplifyType cxt) (baseName nm) tvs (simplifyConName con) ds
simplify dec =
  error $ "Generate.DSL.Project.simplify: not a data type decalaration: " ++ show dec

addFunctorDeriving (TH.DataD cxt nm tvs cons ds) =
  TH.DataD cxt nm tvs cons (nub (TH.mkName "Functor":ds))
addFunctorDeriving (TH.NewtypeD cxt nm tvs con ds) =
  TH.NewtypeD cxt nm tvs con (nub (TH.mkName "Functor":ds))
addFunctorDeriving dec =
  error $ "Generate.DSL.Project.addFunctorDeriving: not a data type declaration: " ++ show dec

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


synthesizeInstructions :: SrcLoc -> [Name] -> Mop [TH.Dec]
synthesizeInstructions at =
  fmap concat <$> mapM (createInstructions at <=< typeInfo <=< reify)
  . map convertNm

createInstructions :: SrcLoc -> THInfo -> Mop [TH.Dec]
createInstructions sl THInfo{..} =
  if length infoConstructors > 1
  then createClosedSymbol sl THInfo{..}
  else createOpenSymbol sl THInfo{..}

createClosedSymbol :: SrcLoc -> THInfo -> Mop [TH.Dec]
createClosedSymbol sl THInfo{..} = do
  log Debug (show THInfo{..})
  return [TH.DataD cxt nm tvs cons derives]
  where
    ict = zip infoConstructors infoTerms
    cxt = []
    nm = TH.mkName . coize . TH.nameBase $ infoName
    recnm = nm
    tvs = map TH.PlainTV infoParams
    cons =
      let vsts = flip map ict $ \((mayCxt,(nm',_)),(_,ts)) ->
                   (TH.mkName $ uncapitalize $ coize $ TH.nameBase nm'
                   ,TH.NotStrict
                   ,case mayCxt of
                      Nothing        -> buildDual $ map snd ts
                      Just (tvs,cxt) -> TH.ForallT tvs cxt (buildDual $ map snd ts)
                   )
      in [TH.RecC recnm vsts]
    derives = [TH.mkName "Functor"]

buildRecordField :: Maybe ([TH.TyVarBndr],TH.Cxt) -> [TH.Type] -> TH.Type
buildRecordField (Just (tvs',cxt')) vars = TH.ForallT tvs' cxt' (buildRecordField Nothing vars)
buildRecordField _ vars =
  case vars of
    [] -> TH.TupleT 0
    [x] -> x
    _ -> TH.AppT (TH.AppT TH.ArrowT (tuplize (init vars))) (last vars)
  where
    tuplize :: [TH.Type] -> TH.Type
    tuplize xs =
      let n = length xs
      in foldr (\a cont st -> cont (TH.AppT st a))
               (\(TH.AppT l _) -> l)
               (tail xs)
               (TH.AppT (TH.TupleT n) (head xs))

createOpenSymbol :: SrcLoc -> THInfo -> Mop [TH.Dec]
createOpenSymbol sl THInfo{..} = do
  log Debug (show THInfo{..})
  return [TH.DataD cxt nm tvs con derives]
  where
    cxt = []
    nm = TH.mkName . coize . TH.nameBase $ infoName
    tvs = map TH.PlainTV infoParams
    con =
      let sts = flip map (zip infoConstructors infoTerms) $ \((mayCxt,_),(_,ts)) ->
                  case mayCxt of
                    Nothing        -> (TH.NotStrict,buildDual $ map snd ts)
                    Just (tvs,cxt) -> (TH.NotStrict,TH.ForallT tvs cxt (buildDual $ map snd ts))
      in [TH.NormalC nm sts]
    derives = [TH.mkName "Functor"]

buildDual :: [TH.Type] -> TH.Type
buildDual ts =
  let l = length ts
  in case l of
       0 -> error "Empty constructors are not valid symbols. \
                  \Try adding an optional continuation parameter."
       1 -> head ts
       2 -> TH.AppT (TH.AppT TH.ArrowT (head ts)) (last ts)
       _ -> foldr (\a cont st -> cont (TH.AppT st a))
                  (\x -> TH.AppT (TH.AppT TH.ArrowT x) (last ts))
                  (tail $ init ts)
                  (TH.AppT (TH.TupleT (pred l)) (head ts))

synthesizePairings :: SrcLoc -> [Name] -> [THInfo] -> Mop [TH.Dec]
synthesizePairings at syms cosymsTI = do
  symsTI <- mapM (typeInfo <=< reify) $ map convertNm syms
  concat <$> mapM (createPairings at) (zip symsTI cosymsTI)

createPairings :: SrcLoc -> (THInfo,THInfo) -> Mop [TH.Dec]
createPairings at (sym,instr) = return []

synthesizeComputer :: SrcLoc -> [Name] -> Mop [TH.Dec]
synthesizeComputer at =
  fmap concat <$> mapM (createComputer at <=< typeInfo <=< reify)
  . map convertNm

createComputer :: SrcLoc -> THInfo -> Mop [TH.Dec]
createComputer sl THInfo{..} = return []

getDataHead :: String -> String
getDataHead str =
  let dh = takeWhile (/= '\n') str
      is = findIndices (=='(') dh
  in case is of
       [] -> error "Generate.DSL.Project.getDataHead: Unexpectedly small Data head."
       xs -> fst (splitAt (pred (last xs)) dh)

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

convertExistentialFunctor :: String -> String
convertExistentialFunctor str =
  if "forall . " `isInfixOf` str
  then let stripped = replace "forall . " "" str
           dataHead = getDataHead stripped
           funcHead = drop 5 -- remove 'data '
                    $ exhaust ( replace " ->)" ")"
                              . replace " *)" ")"
                              . replace " :: *" ""
                              ) dataHead
       in replace "\n    deriving (Functor)"
                  ("\nderiving instance Functor (" ++ funcHead ++ ")")
                  stripped
  else str

convertExistentialFunctorCo str =
  let strs = lines str
      funcHead = drop 5
               $ exhaust ( replace " ->)" ")"
                         . replace " *)" ")"
                         . replace " :: *" ""
                         ) (head strs)
      funcHead' =
        case findIndices (==' ') funcHead of
          [] -> funcHead
          xs -> fst (splitAt (last xs) funcHead)
  in replace "forall . " "" $
       if any ("=>" `isInfixOf`) strs
       then replace "\n    deriving (Functor)"
                    ("\nderiving instance Functor (" ++ funcHead' ++ ")")
                    str
       else str


{-
pairs_ti :: THInfo -> THInfo -> Q [Dec]
pairs_ti co_ti ti = do
   let (nm,params,_,instr_terms) = fromTHI ti
       (co_nm,co_params,_,_) = fromTHI co_ti
       isClosed = length instr_terms > 1
   pairings <- if isClosed then createClosedPairing else createOpenPairing
   when (length co_params /= length params) $
     error $ "Variable parameter count must match between Instruction and Interpreter to automatically pair.\nParameters: (instruction,interpreter)\n\t"
             ++ show (params,co_params)
   let co_con = if length co_params <= 1
                then TH.ConT co_nm
                else foldl (\st a -> TH.AppT st (VarT a)) (ConT co_nm) $ init co_params
       con = if length params <= 1
             then ConT nm
             else foldl (\st a -> AppT st (VarT a)) (ConT nm) $ init co_params
       inst_head = ConT (mkName "Pairing") `AppT` co_con `AppT` con
   return [ InstanceD [] inst_head pairings ]
   where
     createOpenPairing = do
       let (co_nm,co_params0,_,_) = co_ti
       let (nm,params0,_,_) = ti
       let rename = newName . nameBase
       params <- mapM rename params0
       co_params <- mapM rename co_params0
       return
        [ FunD (mkName "pair")
            [ Clause
                [ VarP (mkName "p")
                , ConP co_nm [VarP $ head co_params]
                , ConP nm (map VarP params)
                ]
                (NormalB (VarE (mkName "p")
                          `AppE` foldl (\st a -> AppE st (VarE a))
                                       (VarE (head co_params))
                                       (init params)
                          `AppE` (VarE $ last params)
                         )
                )
                []
            ]
        ]
     createClosedPairing = do
       let (co_nm,co_params,co_cons,co_terms) = co_ti
           (nm,params,cons,terms) = ti
           (co_con,co_recs) = head co_terms
       when (length co_cons > 1) $ error $
         "Closed pairing does not yet support interpreter variants.\nIf you have a use for this, please suggest it on:\n\tgithub.com/grumply/mop/issues"
       when (snd (head co_cons) /= length cons) $ error $
         "Closed interpreter expected to have " ++
            show (length cons) ++ " records, but found " ++
            show (snd (head co_cons))
       let recPs = map (fromJust . fst) co_recs
       forM (zip (zip [0 :: Int ..] $ map fst cons) terms) $ \((tc,tcon),(_,ts)) -> do
         ts' <- map fst <$> nameTerms ts
         let ps = reverse $ snd $
                    foldl
                      (\(i,st) a -> if i == tc
                                    then (i+1,VarP a:st)
                                    else (i+1,WildP:st)
                      )
                      (0,[])
                      recPs
         return $
          FunD (mkName "pair")
          [ Clause
              [ VarP (mkName "p")
              , ConP co_nm ps
              , ConP tcon $ map VarP ts'
              ]
              (NormalB $
                (VarE $ mkName "p")
                `AppE`
                foldl AppE
                      (VarE $ recPs !! tc)
                      (init $ map VarE ts')
                `AppE`
                VarE (last ts')
              )
              []
          ]

-}
