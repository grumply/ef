{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Generate where

import Control.Applicative
import Control.Arrow

import Control.Monad

import Data.Char

import Data.Data

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Language.Haskell.Exts as HSE

import Data.Monoid

import qualified Product
import qualified Sum

import qualified Data.IntMap as IM

import System.Directory
import System.FilePath
import System.Posix.IO
import System.Posix.Files

import Distribution.Package
import Distribution.PackageDescription

import Control.Monad.Trans.State


--------------------------------------------------------------------------------
-- Entry; runnable from TH

mop :: TH.Q [TH.Dec]
mop = do
  TH.Module pn (TH.ModName mn) <- TH.thisModule
  TH.Loc{..} <- TH.location
  pr <- TH.runIO $ HSE.parseFile loc_filename
  TH.runIO (print =<< findCabalFileFromSourceFile loc_filename)
  case pr of
    HSE.ParseOk a -> analyze a >>= dispatch
    HSE.ParseFailed loc str ->
      fail $ "Could not parse module at "
             ++ show loc ++
             "\nError:\n\t:"
             ++ str

mopDir = ".mop/"

isDataDecl (HSE.DataDecl _ _ _ _ _ _ _) = True
isDataDecl _ = False

data MopState
  = MopState
  { initialDir :: FilePath
  , modifiedFile :: Maybe FilePath
  }

-- finalizing a coalgebra prevents modification of type,
-- but allows modification of method.

type Mop = StateT MopState TH.Q

data ModuleType = Algebra | Coalgebra | Instructions | Interpreters

analyze m@(HSE.Module _ _ _ _ _ _ decls)= do
  let grouped = groupByConstructor decls
      datas = concat $ filter (isDataDecl . head) grouped
  if null datas
  then do
    isInstrs <- checkInstructions m
    if isInstrs
    then return Instructions
    else do
      isInterps <- checkInterpreters m
      if isInterps
      then return Interpreters
      else undefined

  else undefined
  return []

checkInstructions m = undefined

checkInterpreters m = undefined

checkAlgebra m = undefined

checkCoalgebra m = undefined

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

readCabalFile f = do
  pd <- readPackageDescription normal f

determineSrcDir = do
  cwd <- getCurrentDirectory
  return $ findSrcDir cwd
  where
    findSrcDir cwd = verify . reconstruct . takeSrcDir $ deconstruct cwd
      where
        verify x = if length cwd == length x
                   then undefined
                   else undefined
        reconstruct = (</> "src") . foldl1 (</>)
        takeSrcDir  = takeWhile (/= "src")
        deconstruct = splitPath

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

modNameToAlgebra = createModuleName "Algebra"
createAlgebraModule = mopCreateModule "Algebra"

modNameToCoalgebra = createModuleName "Coalgebra"
createCoalgebraModule = mopCreateModule "Coalgebra"

modNameToInstructions = createModuleName "Instructions"
createInstructionsModule = mopCreateModule "Instructions"

modNameToInterpreters = createModuleName "Interpreters"
createInterpretersModule = mopCreateModule "Interpreters"

modNameToPairings = createModuleName "Pairings"
createPairingsModule = mopCreateModule "Pairings"

-- remove the original calling TH expression; this is the cleanup method
-- for successful runs
removeCallSite TH.Loc{..} = undefined

dispatch _ = return []


deriving instance Typeable HSE.Decl
groupByConstructor :: Data a => [a] -> [[a]]
groupByConstructor =
  map ($ [])
  . IM.elems . IM.fromListWith (flip (.))
  . map (\a -> (constrIndex $ toConstr a, (a:)))
