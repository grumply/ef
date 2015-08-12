{-# LANGUAGE RecordWildCards #-}
module Generate.Cabal where

import Generate.Monad

import System.Directory
import System.FilePath

import Distribution.ModuleName as Dist

import Distribution.PackageDescription hiding (Var,Lit)
import Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity as Verbosity

import Distribution.PackageDescription.PrettyPrint
import Distribution.PackageDescription.Configuration
import Distribution.Package
import Distribution.Version

findCabalFile :: FilePath -> IO FilePath
findCabalFile d
  | null d || d == "/" = do
     cwd <- getCurrentDirectory
     error $ "Could not find cabal file above " ++ cwd
  | otherwise = do
     dc <- getDirectoryContents d
     let fs = [ x | x <- dc
                  , takeExtensions x == ".cabal"
                  ]
     if not (null fs)
     then return (d </> head fs)
     else findCabalFile (takeDirectory d)

readCabalFile :: FilePath -> IO GenericPackageDescription
readCabalFile f = readPackageDescription Verbosity.normal f

modifyOtherModules :: ([Dist.ModuleName] -> [Dist.ModuleName]) -> Mop ()
modifyOtherModules f = do
  MopState pkg fp hsem ds <- get
  let pd = packageDescription pkg
      Just lib@Library{..} = library pd
      bi@BuildInfo{..} = libBuildInfo
      oms = f otherModules
      lbi = bi { otherModules = oms }
      pkg' = pkg { packageDescription = pd { library = Just lib { libBuildInfo = lbi } } }
  put $ MopState pkg' fp hsem ds

modifyExposedModules :: ([Dist.ModuleName] -> [Dist.ModuleName]) -> Mop ()
modifyExposedModules f = do
  MopState pkg fp hsem ds <- get
  let pd = packageDescription pkg
      Just lib@Library{..} = library pd
      ems = f exposedModules
      pkg' = pkg { packageDescription = pd { library = Just lib { exposedModules = ems } } }
  put $ MopState pkg' fp hsem ds

addOtherModule m = modifyOtherModules (m:)
removeOtherModule m = modifyOtherModules (filter (/=m))

modifyGenericPackageDescription :: (GenericPackageDescription -> GenericPackageDescription) -> Mop ()
modifyGenericPackageDescription f =
  modify (\st -> st { currentPackageDesc = f $ currentPackageDesc st })

modifyPackageDescription :: (PackageDescription -> PackageDescription) -> Mop ()
modifyPackageDescription f =
  modifyGenericPackageDescription
    (\st -> st { packageDescription = f (packageDescription st) })
