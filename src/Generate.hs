{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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


private :: TH.Q [TH.Dec]
private = return []

public :: TH.Q [TH.Dec]
public = return []

mop :: TH.Q [TH.Dec]
mop = do
  TH.Module pn (TH.ModName mn) <- TH.thisModule
  TH.Loc{..} <- TH.location
  let d = takeDirectory loc_filename
  (pr,cf,c,h,hd) <- TH.runIO $ do
          pr <- HSE.parseFile loc_filename
          cf <- findCabalFile d
          c <- readCabalFile cf
          hd <- findMopDir d
          h <- getMopHistory hd
          return (pr,cf,c,h,hd)
  case pr of
    HSE.ParseOk m -> run TH.Loc{..} m (History h,hd) (c,cf) undefined
    HSE.ParseFailed loc str ->
      fail $ "Could not parse module at "
             ++ show loc ++
             "\nError:\n\t:"
             ++ str

-- run a given set of mop commands within a context of a module, the history for
-- the package and the cabal description.
run :: TH.Loc -> HSE.Module -> (History,FilePath) -> (GenericPackageDescription,FilePath) -> Mop a -> TH.Q a
run TH.Loc{..} m (h,hf) (gpd,cf) x = do
  let mc = MopContext (h,hf) (gpd,cf) m
  ((a,s),c) <- flip runStateT mc . flip runReaderT mc . runWriterT $ runMop x
  TH.runIO $ do
    mapM_ putStrLn s
    when (packageDescChanged gpd (fst $ ms_packageDesc c)) $
      writeGenericPackageDescription
        (snd (ms_packageDesc c))
        (fst (ms_packageDesc c))
    when (moduleChanged m (ms_hseModule c)) $
      writeModuleModifications m (ms_hseModule c)
    when (historyChanged h (fst $ ms_history c)) $
      writeHistoryModifications h (fst $ ms_history c)
  return a

mopDir = ".mop/"

isDataDecl (HSE.DataDecl _ _ _ _ _ _ _) = True
isDataDecl _ = False

bumpMajor :: Version -> Version
bumpMajor _ = undefined

bumpMinor :: Version -> Version
bumpMinor _ = undefined

bumpPatch :: Version -> Version
bumpPatch _ = undefined

data Visibility = Private | Public
  deriving (Read,Show,Eq,Generic)

instance Monoid Visibility where
  mempty                  = Public
  mappend Private _       = Private
  mappend _       Private = Private
  mappend _       _       = Public

data TypeComponent = TypeComponent
  { typeComponentName :: HSE.Name
  , typeComponentContext :: HSE.Context
  , typeComponentVars :: [HSE.TyVarBind]
  , typeComponentType :: [([HSE.TyVarBind],HSE.Context,HSE.Name,HSE.ConDecl)]
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
      { algebraVisibility      :: Visibility
      , algebraName            :: HSE.Name
      , algebraType            :: HSE.Type
      , algebraVars            :: [HSE.TyVarBind]
      , algebraModuleName      :: HSE.ModuleName
      , algebraComponents      :: [TypeComponent]
      }
  | Coalgebra
      { coalgebraVisibility    :: Visibility
      , coalgebraName          :: HSE.Name
      , coalgebraType          :: HSE.Type
      , coalgebraModuleName    :: HSE.ModuleName
      , coalgebraComponents    :: [TypeComponent]
      }
  | Instructions
      { instructionsVisibility :: Visibility
      , instructionsType       :: HSE.Type
      , instructionsModuleName :: HSE.ModuleName
      , instructionsComponents :: [FuncComponent]
      }
  | Interpreters
      { interpretersVisibility :: Visibility
      , interpretersType       :: HSE.Type
      , interpretersModuleName :: HSE.ModuleName
      , interpretersComponents :: [FuncComponent]
      }
  | Pairings
      { pairingsVisibility     :: Visibility
      , pairingsModuleName     :: HSE.ModuleName
      , pairingsComponents     :: [Pair]
      }
  deriving (Eq,Read,Show,Generic)

type VersionedPackage = (Version,[Component],GenericPackageDescription)

data History = History [(Version,IO VersionedPackage)]
instance Eq History where
  (History xs) == (History ys) = map fst xs == map fst ys

allLocalInstances :: HSE.QName -> [HSE.Decl] -> [HSE.Deriving]
allLocalInstances qn decls = undefined

data MopContext = MopContext
  { ms_history     :: (History,FilePath)
  , ms_packageDesc :: (GenericPackageDescription,FilePath)
  , ms_hseModule   :: HSE.Module
  }

newtype Mop a = Mop
  { runMop :: WriterT [String] (ReaderT MopContext (StateT MopContext TH.Q)) a }
  deriving ( Functor, Applicative, Monad
           , MonadState MopContext
           , MonadReader MopContext
           , MonadWriter [String]
           )

algebra :: Mop [TH.Dec]
algebra = undefined

writeModuleModifications :: HSE.Module -> HSE.Module -> IO ()
writeModuleModifications old new = undefined

writeHistoryModifications :: History -> History -> IO ()
writeHistoryModifications old new = undefined

packageDescChanged x y = not $ (==) x y

historyChanged x y = not $ (==) x y

moduleChanged x y = not $ (==) x y

modifyHistory :: (History -> History) -> Mop ()
modifyHistory f = modify (\st -> st { ms_history = first f $ ms_history st })

modifyGenericPackageDescription :: (GenericPackageDescription -> GenericPackageDescription) -> Mop ()
modifyGenericPackageDescription f =
  modify (\st -> st { ms_packageDesc = first f $ ms_packageDesc st })

modifyPackageDescription :: (PackageDescription -> PackageDescription) -> Mop ()
modifyPackageDescription f =
  modifyGenericPackageDescription
    (\st -> st { packageDescription = f (packageDescription st) })

modifyModule :: (HSE.Module -> HSE.Module) -> Mop ()
modifyModule f =
  modify (\st -> st { ms_hseModule = f $ ms_hseModule st })

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

findMopDir d
  | null d || d == "/" = do
     cwd <- getCurrentDirectory
     error $ "Could not find .mop directory in or above " ++ cwd
  | otherwise = do
     dc <- filterValidDirectories =<< getDirectoryContents d
     if ".mop" `elem` dc
     then return (d </> ".mop")
     else findMopDir (takeDirectory d)

getMopHistory d = do
  dc <- getDirectoryContents d
  let mops = filter (".mop" `isSuffixOf`) dc
      mvs = mapMaybe breakNameVersion mops
  return $ map (\(nm,v) -> (v,read <$> readFile (d </> (nm ++ "_" ++ show v) <.> "mop"))) mvs
  where
    breakNameVersion (reverse . drop 4 . reverse -> str) =
      case break (=='_') str of
        ([],_) -> Nothing
        (_,[]) -> Nothing
        (nm,_:ver) -> Just (nm,read ver)

sourceDirectory pkg =
  let Just Library{..} = library pkg
      BuildInfo{..} = libBuildInfo
  in hsSourceDirs

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

removeCallSite TH.Loc{..} = undefined

groupByConstructor :: Data a => [a] -> [[a]]
groupByConstructor =
  map ($ [])
  . IM.elems . IM.fromListWith (flip (.))
  . map (\a -> (constrIndex $ toConstr a, (a:)))
