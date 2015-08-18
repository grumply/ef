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
      symbolsModule          = Dist.toFilePath symbolsModuleName
      absSymbolsModule       = md </> symbolsModule <.> "hs"

      tapeModuleName         = Dist.fromString ("Tape." ++ symbolsName)
      tapeModule             = Dist.toFilePath tapeModuleName
      absTapeModule          = md </> tapeModule <.> "hs"

      instructionsModuleName = Dist.fromString ("Instructions." ++ symbolsName)
      instructionsModule     = Dist.toFilePath instructionsModuleName
      absInstructionsModule  = md </> instructionsModule <.> "hs"

      computerModuleName     = Dist.fromString ("Computer." ++ symbolsName)
      computerModule         = Dist.toFilePath computerModuleName
      absComputerModule      = md </> computerModule <.> "hs"

  guaranteeSourceDir mopDirectory

  createEmptyModule symbolsModuleName      mopDirectory >>= writeModule
  createEmptyModule tapeModuleName         mopDirectory >>= writeModule
  createEmptyModule instructionsModuleName mopDirectory >>= writeModule
  createEmptyModule computerModuleName     mopDirectory >>= writeModule

  guaranteeImport mopTape         absTapeModule
  guaranteeImport mopComputer     absComputerModule
  guaranteeImport mopInstructions absInstructionsModule
  guaranteeImport mopSymbols      absSymbolsModule

  tapesl         <- findEnd absTapeModule
  computersl     <- findEnd absComputerModule
  instructionssl <- findEnd absInstructionsModule
  symbolssl      <- findEnd absSymbolsModule

  tape_splices         <- synthesizeTape tapesl symbolNames
  computer_splices     <- return []
  instructions_splices <- return []
  symbols_splices      <- return []

  mapM_ addExposedModule
    [ symbolsModuleName
    , tapeModuleName
    , instructionsModuleName
    , computerModuleName
    ]

  mapM_ (\(sl,spls) -> mapM_ (spliceAtEnd sl) spls)
    [ (tapesl        ,tape_splices         )
    , (computersl    ,computer_splices     )
    , (instructionssl,instructions_splices )
    , (symbolssl     ,symbols_splices      )
    ]

  mapM_ (flip transitionExtension absTapeModule)
    [ noMonomorphismRestrictionPragma
    , deriveFunctorPragma
    , flexibleContextsPragma
    , typeOperatorsPragma
    ]

  modifyVersion incrementMinor -- only adding: minor

  void (unsplice (srcLine at) 1 (srcFilename at))

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
synthesizeTape at = fmap concat <$> mapM (synthesize at <=< typeInfo <=< reify)
                  . map convertNm

synthesize :: SrcLoc -> THInfo -> Mop [Decl]
synthesize sl THInfo{..} = do
    symbols   <- createSymbols      sl THInfo{..}
    instrs    <- createInstructions sl THInfo{..}
    pairings  <- createPairings     sl THInfo{..} instrs
    computers <- createComputers    sl instrs
    return (symbols ++ instrs ++ pairings ++ computers)

createSymbols :: SrcLoc -> THInfo -> Mop [Decl]
createSymbols sl THInfo{..} = fmap concat <$>
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

{-
THInfo
  { infoName = A
  , infoParams = [s_1627411019,k_1627411020]
  , infoConstructors = [(Nothing,(A,2))]
  , infoTerms = [(Main.A,[(Nothing,AppT (AppT ArrowT (VarT s_1627411019)) (VarT k_1627411020))
                         ,(Nothing,VarT k_1627411020)
                         ]
                 )
                ]
  }

[FunBind
  [Match
    (SrcLoc "src/Main.hs" 17 1)
    (Ident "a")
    [PVar (Ident "sk")]
    Nothing
    (UnGuardedRhs
      (App (Var (UnQual (Ident "liftF")))
           (Paren (App (Var (UnQual (Ident "inj")))
                       (Paren (App (App (Con (UnQual (Ident "A")))
                                        (Var (UnQual (Ident "sk")))
                                   )
                                   (Var (Special UnitCon))
                              )
                       )
                  )
           )
      )
    )
    (BDecls [])
  ]
]

DataDecl SrcLoc DataOrNew Context Name [TyVarBind] [QualConDecl] [Deriving]

-}

createInstructions :: SrcLoc -> THInfo -> Mop [Decl]
createInstructions sl THInfo{..} = return []

createPairings :: SrcLoc -> THInfo -> [Decl] -> Mop [Decl]
createPairings sl THInfo{..} instrs = return []

createComputers :: SrcLoc -> [Decl] -> Mop [Decl]
createComputers sl instrs = return []

buildContext :: (Maybe ([TH.TyVarBndr],TH.Cxt),(TH.Name,Int)) -> Context
buildContext _ = undefined

buildTyVars :: (Maybe ([TH.TyVarBndr],TH.Cxt),(TH.Name,Int)) -> [TyVarBind]
buildTyVars _ = undefined

buildCoConstructors :: TH.Name -> [TH.Type] -> [QualConDecl]
buildCoConstructors _ _ = undefined

coConstructorDerives :: [Deriving]
coConstructorDerives = []

isHigherKinded (TH.AppT _ _) = True
isHigherKinded _ = False

varNames :: [Name]
varNames =
  let strings = [c:s | s <- "":strings, c <- ['a'..]]
  in map Ident strings

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

deduplicateNames :: [Name] -> [Name]
deduplicateNames ns = renameDuplicates ns (findDuplicates ns)
  where
    findDuplicates :: [Name] -> [Name]
    findDuplicates = concat . map snd . filter ((> 1) . fst) . map (\x -> (length x,x)) . groupBy (==)
    renameDuplicates :: [Name] -> [Name] -> [Name]
    renameDuplicates orig [] = orig
    renameDuplicates orig (dup:dups) = renameDuplicates (renameDuplicate orig dup) dups
    renameDuplicate :: [Name] -> Name -> [Name]
    renameDuplicate orig dup = foldr (\a cont (n,dup,acc) -> cont $
                                        if dup == a
                                        then (succ n,dup,rename n a:acc)
                                        else (n,dup,a:acc)
                                     )
                                     (\(_,_,acc) -> acc)
                                     orig
                                     (0,dup,[])
    rename :: Int -> Name -> Name
    rename n (Ident nm) = Ident (nm ++ show n)

makeVar :: TH.Name -> Pat
makeVar (TH.nameBase -> nm) = PVar (Ident nm)

coize :: String -> String
coize [] = error "Could not coize empty string."
coize str@(x:_) = if isLower x then "co" ++ str else "Co" ++ str
