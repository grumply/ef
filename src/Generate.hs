{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Distribution.Verbosity
import Distribution.Version

import Control.Monad.Trans.State

private :: TH.Q [TH.Dec]
private = return []

public :: TH.Q [TH.Dec]
public = return []

mop :: TH.Q [TH.Dec]
mop = do
  TH.Module pn (TH.ModName mn) <- TH.thisModule
  TH.Loc{..} <- TH.location
  pr <- TH.runIO $ HSE.parseFile loc_filename
  --TH.runIO (print =<< readCabalFile =<< findCabalFileFromSourceFile loc_filename)
  case pr of
    HSE.ParseOk a -> do
      TH.runIO $ print a
      TH.runIO (putStrLn "Here")
      return []

    HSE.ParseFailed loc str ->
      fail $ "Could not parse module at "
             ++ show loc ++
             "\nError:\n\t:"
             ++ str

mopDir = ".mop/"

isDataDecl (HSE.DataDecl _ _ _ _ _ _ _) = True
isDataDecl _ = False

bumpMajor :: Version -> Version
bumpMajor _ = undefined

bumpMinor :: Version -> Version
bumpMinor _ = undefined

bumpPatch :: Version -> Version
bumpPatch _ = undefined

data AlgebraType = MixedAlgebra | OpenAlgebra | ClosedAlgebra
  deriving (Read,Show,Eq,Generic)

data CoalgebraType = OpenCoalgebra | ClosedCoalgebra
  deriving (Read,Show,Eq,Generic)

data Visibility = Private | Public
  deriving (Read,Show,Eq,Generic)

instance Monoid Visibility where
  mempty                  = Public
  mappend Private _       = Private
  mappend _       Private = Private
  mappend _       _       = Public

data TypeComponent = TypeComponent
  { typeComponentName :: HSE.Name
  , typeComponentType :: HSE.Type
  , typeComponentDerives :: [HSE.Name]
  } deriving (Read,Show,Generic)

data FuncComponent = FuncComponent
  { funcComponentName :: HSE.Name
  , funcComponentType :: HSE.Type
  , funcComponentImpl :: HSE.Decl
  } deriving (Read,Show,Generic)

data Pair = Pair
  { pairInstruction :: HSE.Name
  , pairInterpreter :: HSE.Name
  , pairInstance    :: HSE.Decl
  } deriving (Read,Show,Generic)

data Component
  = Algebra
      { algebraVisibility      :: Visibility
      , algebraAlgebraType     :: AlgebraType
      , algebraName            :: HSE.Name
      , algebraType            :: HSE.Type
      , algebraModuleName      :: HSE.ModuleName
      , algebraComponents      :: [TypeComponent]
      }
  | Coalgebra
      { coalgebraVisibility    :: Visibility
      , coalgebraCoalgebraType :: CoalgebraType
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
  deriving (Read,Show,Generic)

type VersionedPackage = (Version,[Component],GenericPackageDescription)

data History = History [Version]
  deriving (Read,Show,Generic)

analyze config m@(HSE.Module _ (HSE.ModuleName mn) _ _ _ _ _) = do
  case break (=='.') mn of
    ("Devel",[]) -> undefined
    ("Alg",_)    -> analyzeAlgebra m
    ("Coalg",_)  -> undefined
    ("Instr",_)  -> undefined
    ("Interp",_) -> undefined
    ("Pair",_)   -> undefined

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

analyzeAlgebra pkg m@(HSE.Module _ (HSE.ModuleName nm) _ _ _ _ decls) =
  let v = getVisibility m
  in Algebra v

checkInstructions m = undefined

checkInterpreters m = undefined

checkAlgebra m = undefined

checkCoalgebra m = undefined

dispatch _ = return []

findCabalFileFromSourceFile = findCabalFile . takeDirectory

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

readCabalFile :: FilePath -> IO PackageDescription
readCabalFile f = flattenPackageDescription <$> readPackageDescription normal f

makePrivate m = addOtherModule m . removeExposedModule m
makePublic m  = addExposedModule m . removeOtherModule m

addOtherModule m pkg =
  let Just lib@Library{..} = library pkg
      bi@BuildInfo{..} = libBuildInfo
      oms = m:otherModules
      lbi = bi { otherModules = oms }
  in pkg { library = Just lib { libBuildInfo = lbi } }

removeOtherModule m pkg =
  let Just lib@Library{..} = library pkg
      bi@BuildInfo{..} = libBuildInfo
      oms = filter (/=m) otherModules
      lbi = bi { otherModules = oms }
  in pkg { library = Just lib { libBuildInfo = lbi } }

addExposedModule m pkg =
  let Just lib@Library{..} = library pkg
      ems = m:exposedModules
  in pkg { library = Just lib { exposedModules = ems } }

removeExposedModule m pkg =
  let Just lib@Library{..} = library pkg
      ems = filter (/=m) exposedModules
  in pkg { library = Just lib { exposedModules = ems } }

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
      else concat <$> mapM (findModules' . (sd </>)) ds
      where
        findModules' d = do
          dc <- filterValidDirectories =<< getDirectoryContents d
          rest <- concat <$> mapM (findModules' . (d </>)) dc
          return (dc ++ rest)

filterValidDirectories = filterM $ \x ->
  liftM ((not ("." `isPrefixOf` x)) &&) (doesDirectoryExist x)

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
