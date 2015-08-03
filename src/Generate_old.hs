{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Generate where

import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.Char
import Data.Data
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Data.Monoid

import GHC.Generics

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Language.Haskell.Exts as HSE

import Derives
import qualified Product
import qualified Sum

import System.Directory
import System.FilePath
import System.Posix.IO
import System.Posix.Files

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.Verbosity
import Distribution.Version

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

data Visibility = Private | Public
  deriving (Read,Show,Eq,Generic)

instance Monoid Visibility where
  mempty                  = Public
  mappend Private _       = Private
  mappend _       Private = Private
  mappend _       _       = Public

data TypeComponent = TypeComponent
  { typeComponentName    :: HSE.Name
  , typeComponentContext :: HSE.Context
  , typeComponentVars    :: [HSE.TyVarBind]
  , typeComponentType    :: [([HSE.TyVarBind],HSE.Context,HSE.Name,HSE.ConDecl)]
  , typeComponentDerives :: [HSE.Deriving]
  } deriving (Eq,Read,Show,Generic)
-- gather instances and derives

data FuncComponent = FuncComponent
  { funcComponentName :: HSE.Name
  , funcComponentType :: HSE.Type
  , funcComponentImpl :: HSE.Decl
  } deriving (Eq,Read,Show,Generic)

data Pair = Pair
  { pairInstruction :: HSE.Name
  , pairInterpreter :: HSE.Name
  , pairInstance    :: HSE.Decl
  } deriving (Eq,Read,Show,Generic)

data Component
  = Algebra
      { algebraName       :: HSE.Name
      , algebraType       :: HSE.Type
      , algebraVars       :: [HSE.TyVarBind]
      , algebraModuleName :: HSE.ModuleName
      , algebraComponents :: [TypeComponent]
      }
  | Coalgebra
      { coalgebraName       :: HSE.Name
      , coalgebraType       :: HSE.Type
      , coalgebraVars       :: [HSE.TyVarBind]
      , coalgebraModuleName :: HSE.ModuleName
      , coalgebraComponents :: [TypeComponent]
      }
  | Instructions
      { instructionsType       :: HSE.Type
      , instructionsModuleName :: HSE.ModuleName
      , instructionsComponents :: [FuncComponent]
      }
  | Interpreters
      { interpretersType       :: HSE.Type
      , interpretersModuleName :: HSE.ModuleName
      , interpretersComponents :: [FuncComponent]
      }
  | Pairings
      { pairingsModuleName :: HSE.ModuleName
      , pairingsComponents :: [Pair]
      }
  deriving (Eq,Read,Show,Generic)

type VersionedComponents = (Version,[Component],GenericPackageDescription)

newtype History = History { history :: [(Version,IO VersionedComponents)] }
instance Eq History where
  (History xs) == (History ys) = map fst xs == map fst ys

-- The general mop context contains a history of the current package,
-- the package description representing the cabal file for the current package,
-- and a haskell-src-exts Module representing the current mop invocation context.
data MopContext = MopContext
  { mc_history     :: (History,FilePath)
  , mc_hseModule   :: HSE.Module
  }

data MopState = MopState
  { ms_components :: [Component]
  , ms_package    :: GenericPackageDescription
  , ms_hseModule  :: HSE.Module
  }

-- Mop is an execution context containing a MopContext state for the current
-- module and package. The same state is available in a reader context for
-- diffing. Writer as a list of strings for logging.
newtype Mop a = Mop
  { runMop :: WriterT [String] (ReaderT MopContext (StateT ([Component],) TH.Q)) a }
  deriving ( Functor, Applicative, Monad
           , MonadState MopContext
           , MonadReader MopContext
           , MonadWriter [String]
           )

-- saving implementation for phase 2
-- private :: TH.Q [TH.Dec]
-- private = return []
-- public :: TH.Q [TH.Dec]
-- public = return []



mop :: TH.Q [TH.Dec]
mop = do
  TH.Module pn (TH.ModName mn) <- TH.thisModule
  TH.Loc{..}                   <- TH.location

  let l = loc_filename
      d = takeDirectory l

  (pm,h,c) <- TH.runIO $ do
    pm     <- HSE.parseFile l

    md     <- findMopDir d
    mh     <- getMopHistory md

    cf     <- findCabalFile d
    c      <- readCabalFile cf

    return (pm,(mh,md),(c,cf))

  case pm of
    HSE.ParseOk m           -> run TH.Loc{..} m h c algebra
    HSE.ParseFailed loc str -> fail $
      "Could not parse module at " ++ show loc ++ "\nError:\n\t:" ++ str

run  :: TH.Loc
     -> HSE.Module
     -> (History,FilePath)
     -> (GenericPackageDescription,FilePath)
     -> Mop a
     -> TH.Q a
run TH.Loc{..} m (h,hf) (gpd,cf) x = do
  let mc = MopContext (h,hf) (gpd,cf) m
  ((a,s),c) <- flip runStateT mc . flip runReaderT mc . runWriterT $ runMop x
  TH.runIO $ do
    mapM_ putStrLn s
    when (packageDescChanged gpd (fst $ mc_packageDesc c)) $
      flip (uncurry writeCabalModifications) (mc_packageDesc c)
    when (moduleChanged m (mc_hseModule c)) $
      writeModuleModifications m (mc_hseModule c)
    when (historyChanged h (fst $ mc_history c)) $
      writeHistoryModifications h (fst $ mc_history c)
  return a


algebra :: Mop [TH.Dec]
algebra = undefined

isDataDecl (HSE.DataDecl _ _ _ _ _ _ _) = True
isDataDecl _ = False

bumpMajor :: Version -> Version
bumpMajor _ = undefined

bumpMinor :: Version -> Version
bumpMinor _ = undefined

bumpPatch :: Version -> Version
bumpPatch _ = undefined

allLocalInstances :: HSE.QName -> [HSE.Decl] -> [HSE.Deriving]
allLocalInstances qn decls = undefined

writeModuleModifications :: HSE.Module -> HSE.Module -> IO ()
writeModuleModifications old new = undefined




moduleChanged x = not . (x ==)

runCoalgebraMop = undefined

getVisibility m@(HSE.Module _ _ _ _ _ _ decls) =
  let spl = catMaybes $ flip map decls $ \d ->
              case d of
                HSE.SpliceDecl _ (HSE.Var (HSE.UnQual (HSE.Ident x))) ->
                  case x of
                    "private" -> Just Private
                    "public" -> Just Public
                    _ -> Nothing
                _ -> Nothing
  in case spl of
       [] -> Public
       [x] -> x
       _ -> error "Found multiple visibility declarations."

analyzeAlgebra :: PackageDescription -> HSE.Module -> Component
analyzeAlgebra pkg m@(HSE.Module _ nm _ _ _ _ decls) =
  let (algebraName,algebraVars,algebraType) = extractAlgebra m
      algebraVisibility = getVisibility m
      algebraModuleName = nm
      algebraComponents = makeAlgebraComponents decls
  in Algebra{..}

extractAlgebra :: HSE.Module -> (HSE.Name,[HSE.TyVarBind],HSE.Type)
extractAlgebra m@(HSE.Module _ nm _ _ _ _ decls) =
  extract $ mapMaybe justSumType decls
  where
    justSumType t@(HSE.TypeDecl _ _ _ st@(isAlgebra -> True)) = Just t
    justSumType _ = Nothing

    extract [] = error $ "Could not find sum of instructions in " ++ show nm
    extract (x:xs) =
      case x of
        HSE.TypeDecl _ nm tvars ty -> (nm,tvars,ty)
        _ -> extract xs

isAlgebra (HSE.TyInfix _ (HSE.UnQual (HSE.Symbol ":+:")) _) = True
isAlgebra _ = False

isCoalgebra (HSE.TyInfix _ (HSE.UnQual (HSE.Symbol ":*:")) _) = True
isCoalgebra _ = False

makeAlgebraComponents :: [HSE.Decl] -> [TypeComponent]
makeAlgebraComponents ds = flip mapMaybe ds $ \d ->
  case d of
    HSE.DataDecl _ _ _ cxt nm _ _ -> undefined

dispatch _ = return []


removeCallSite TH.Loc{..} = undefined

groupByConstructor :: Data a => [a] -> [[a]]
groupByConstructor =
  map ($ [])
  . IM.elems . IM.fromListWith (flip (.))
  . map (\a -> (constrIndex $ toConstr a, (a:)))

--------------------------------------------------------------------------------
-- Cabal configuration: locate, read, diff, write

findCabalFile d
  | null d || d == "/" = do
     cwd <- getCurrentDirectory
     error $ "Could not find cabal file above " ++ cwd
  | otherwise = do
     dc <- getDirectoryContents d
     let fs = filter ((==) ".cabal" . takeExtensions) dc
     if not (null fs)
     then return (d </> head fs)
     else findCabalFile (takeDirectory d)

readCabalFile :: FilePath -> IO GenericPackageDescription
readCabalFile f = readPackageDescription normal f

packageDescChanged x = not . (x ==)

writeCabalModifications fp c = writeGenericPackageDescription fp c

--------------------------------------------------------------------------------
-- Mop history log: locate, read, diff write

mopDir = ".mop/"

findMopDir d
  | null d || d == "/" = promptForNewMopDir
  | otherwise = do
     dc <- filterValidDirectories =<< getDirectoryContents d
     if mopDir `elem` dc
     then return (d </> mopDir)
     else findMopDir (takeDirectory d)

promptForNewMopDir = do
  cd <- takeDirectory <$> findCabalFile
  cwd <- getCurrentDirectory
  putStrLn $ "Could not find .mop directory in or above " ++ cwd
  putStrLn $ "Create new .mop directory at " ++ cd ++ "? [Y/n]"
  ln <- getLine
  case map toLower ln of
    xs | "y" `isPrefixOf` xs -> createDirectory (cd </> mopDir)
       | otherwise -> error "No mop history directory - exiting."

createVerDir :: FilePath -> Version -> IO ()
createVerDir fp v = createDirectory (makeVerDir fp v)

makeVerDir :: FilePath -> Version -> FilePath
makeVerDir mopd v = mopd </> show v

makeComponentFilePath :: FilePath -> Version -> Component -> FilePath
makeComponentFilePath fp v c = makeVerDir fp v </> makeComponentName c

makeComponentName :: Component -> FilePath
makeComponentName Algebra{..}      = moduleNameToMopFileName algebraModuleName
makeComponentName Coalgebra{..}    = moduleNameToMopFileName coalgebraModuleName
makeComponentName Instructions{..} = moduleNameToMopFileName instructionsModuleName
makeComponentName Interpreters{..} = moduleNameToMopFileName interpretersModuleName
makeComponentName Pairings{..}     = moduleNameToMopFileName pairingsModuleName

moduleNameToMopFileName :: HSE.ModuleName -> FilePath
moduleNameToMopFileName (HSE.ModuleName mn) = makeValid mn

getMopHistory d = do
  dc <- getDirectoryContents d
  let mops = filter (mopDir `isSuffixOf`) dc
      mvs = mapMaybe breakMopHistoryFileName mops
      lazyRead nm v = read <$> readFile (d </> (nm ++ "_" ++ show v) <.> "mop")
  return $ History $ map (\(ver,nm) -> (ver,lazyRead nm ver)) mvs

historyChanged x = not . (x ==)

writeHistoryModifications :: FilePath -> History -> History -> IO ()
writeHistoryModifications mopd old new = writeDiffs diffs
  where
    diffs :: ([(Version,IO VersionedComponents)]
             ,[(Version,IO VersionedComponents)]
             )
    diffs =
      let mark b (x,y) = (b,x,y)

          os           = map (mark False) (history old)
          ns           = map (mark True ) (history new)
          vs           = os ++ ns

          groupByVersion  = groupBy (\(_,y,_) (_,y',_) -> y == y')
          filterUnmatched = map head . filter ((==1) . length)
          filterNew       = filter  (\(_,y,_) -> y)
          filterOld       = filter  (\(_,y,_) -> not y)
          removeTag       = map     (\(x,_,y) -> (x,y))

          createVersions  = removeTag . filterNew . filterUnmatched . groupByVersion
          deleteVersions  = removeTag . filterOld . filterUnmatched . groupByVersion

      in (createVersions vs,deleteVersions vs)

    writeDiffs (creates,deletes) = do
      createVersions creates
      deleteVersions deletes

    createVersions :: [(Version,IO VersionedComponents)] -> IO ()
    createVersions vvps = forM_ vvps $ \(v,iovc) -> do
      let d = makeVerDir mopd v
      vc <- iovc
      let l = length vc
      l `seq` do
      let fp = createMopHistoryFilePath mopd v vc
      undefined

    deleteVersions :: [(Version,IO VersionedComponents)] -> IO ()
    deleteVersions vvps = forM_ vvps $ \(v,iovc) -> do
      let d = makeVerDir mopd v
      vc <- iovc
      let l = length vc
      l `seq` do
      let fp = createMopHistoryFilePath mopd v vc

breakMopHistoryFileName FilePath -> Maybe (Version,String)
breakMopHistoryFileName (reverse . drop 4 . reverse -> str) =
  case break (=='_') str of
    ([],_) -> Nothing
    (_,[]) -> Nothing
    (nm,_:ver) -> Just (read ver,nm)

createMopHistoryFilePath :: FilePath -> Version -> VersionedComponents -> IO ()
createMopHistoryFilePath dir v vc = dir </> show v </> makeComponentName vc <.> "mop"

--------------------------------------------------------------------------------
-- Module and directory manipulation

moduleDirectory :: String -> String
moduleDirectory = foldl1 (</>) . break [] []
  where
    break acc cur [] = reverse (reverse cur:acc)
    break acc cur ('.':xs) = break (reverse cur:acc) [] xs
    break acc cur (x:xs) = break acc (x:cur) xs

createModuleName x (HSE.ModuleName str) = HSE.ModuleName (x ++ '.':str)

mopCreateDir dir srcDir (HSE.ModuleName x) = do
  let d = srcDir </> dir </> moduleDirectory x
  de <- doesDirectoryExist d
  unless de (createDirectory d)
  return d

mopCreateModule dir srcDir mn@(HSE.ModuleName x) = do
  d <- mopCreateDir dir srcDir mn
  let f = d </> dir <.> "hs"
  fe <- doesFileExist f
  unless fe (createFile f stdFileMode >>= closeFd)

findModules ty pkg = concat <$> mapM go (sourceDirectory pkg)
  where
    go sd = do
      dc <- getDirectoryContents sd
      let ds = filter (==ty) dc
      if null ds
      then return []
      else filter (".hs" `isSuffixOf`) . concat <$> mapM (findModules' . (sd </>)) ds
      where
        findModules' d = do
          dc <- filterValidDirectories =<< getDirectoryContents d
          rest <- concat <$> mapM (findModules' . (d </>)) dc
          return (dc ++ rest)

filterValidDirectories = filterM $ \x ->
  liftM ((not ("." `isPrefixOf` x)) &&) (doesDirectoryExist x)

modifyOtherModules f pkg =
  let Just lib@Library{..} = library pkg
      bi@BuildInfo{..} = libBuildInfo
      oms = f otherModules
      lbi = bi { otherModules = oms }
  in pkg { library = Just lib { libBuildInfo = lbi } }

modifyExposedModules f pkg =
  let Just lib@Library{..} = library pkg
      ems = f exposedModules
  in pkg { library = Just lib { exposedModules = ems } }

addOtherModule m = modifyOtherModules (m:)
removeOtherModule m = modifyOtherModules (filter (/=m))

addExposedModule m = modifyExposedModules (m:)
removeExposedModule m = modifyExposedModules (filter (/=m))

makePrivate m = addOtherModule m . removeExposedModule m
makePublic m  = addExposedModule m . removeOtherModule m

modNameToAlgebra = createModuleName "Alg"
createAlgebraModule = mopCreateModule "Alg"
gatherAlgebraModules = findModules "Alg"

modNameToCoalgebra = createModuleName "Coalg"
createCoalgebraModule = mopCreateModule "Coalg"
gatherCoalgebraModules = findModules "Coalg"

modNameToInstructions = createModuleName "Instr"
createInstructionsModule = mopCreateModule "Instr"
gatherInstructionsModules = findModules "Instr"

modNameToInterpreters = createModuleName "Interp"
createInterpretersModule = mopCreateModule "Interp"
gatherInterpretersModules = findModules "Interp"

modNameToPairings = createModuleName "Pair"
createPairingsModule = mopCreateModule "Pair"
gatherPairingsModules = findModules "Pair"


--------------------------------------------------------------------------------
-- Mop state accessors/modifiers functions

sourceDirectory :: Mop FilePath
sourceDirectory = do
  MopContext{..} <- ask
  let pkg = mc_packageDesc
      Just Library{..} = library pkg
      BuildInfo{..} = libBuildInfo
  return hsSourceDirs

modifyHistory :: (History -> History) -> Mop ()
modifyHistory f = modify (\st -> st { mc_history = first f $ mc_history st })

modifyGenericPackageDescription :: (GenericPackageDescription -> GenericPackageDescription) -> Mop ()
modifyGenericPackageDescription f =
  modify (\st -> st { mc_packageDesc = first f $ mc_packageDesc st })

modifyPackageDescription :: (PackageDescription -> PackageDescription) -> Mop ()
modifyPackageDescription f =
  modifyGenericPackageDescription
    (\st -> st { packageDescription = f (packageDescription st) })

modifyModule :: (HSE.Module -> HSE.Module) -> Mop ()
modifyModule f =
  modify (\st -> st { mc_hseModule = f $ mc_hseModule st })

modifyModuleName :: (HSE.ModuleName -> HSE.ModuleName) -> Mop ()
modifyModuleName x =
  modifyModule (\(HSE.Module a b c d e f g) -> HSE.Module a (x b) c d e f g)

modifyExports :: (Maybe [HSE.ExportSpec] -> Maybe [HSE.ExportSpec]) -> Mop ()
modifyExports x =
  modifyModule (\(HSE.Module a b c d e f g) -> HSE.Module a b c d (x e) f g)

modifyImports :: ([HSE.ImportDecl] -> [HSE.ImportDecl]) -> Mop ()
modifyImports x =
  modifyModule (\(HSE.Module a b c d e f g) -> HSE.Module a b c d e (x f) g)

modifyPragmas :: ([HSE.ModulePragma] -> [HSE.ModulePragma]) -> Mop ()
modifyPragmas x =
  modifyModule (\(HSE.Module a b c d e f g) -> HSE.Module a b (x c) d e f g)

modifyDecls :: ([HSE.Decl] -> [HSE.Decl]) -> Mop ()
modifyDecls x =
  modifyModule (\(HSE.Module a b c d e f g) -> HSE.Module a b c d e f (x g))
