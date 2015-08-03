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
import Data.Typeable

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

--------------------------------------------------------------------------------
-- Types for configuration of mops and buckets, bucket representations, exec.
-- context (TH location/module), execution state (buckets and cabal file), and
-- history (function from version to execution state), logging, and the Mop
-- execution context.

data MopName = MopName String
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data BucketName = BucketName String
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data BucketVersion = BucketVersion Version
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data MopConfiguration = MopConfiguration
  { mopName             :: MopName
  , buckets             :: [(BucketName,BucketVersion)]
  } deriving (Show,Read,Eq,Data,Typeable)

data BucketConfiguration = BucketConfiguration
  { bucketName          :: BucketName
  , bucketVersion       :: BucketVersion
  } deriving (Show,Read,Eq,Data,Typeable)

-- Bucket is a subset of a cabal package. Multiple `bucket`S may be used
-- in a single cabal package.
data Bucket = Bucket
  { bucketConfig        :: BucketConfiguration
  , algebraModules      :: [HSE.Module]
  , coalgebraModules    :: [HSE.Module]
  , instructionsModules :: [HSE.Module]
  , interpretersModules :: [HSE.Module]
  , pairingsModules     :: [HSE.Module]
  , otherModules        :: [HSE.Module]
  } deriving (Show,Read,Eq,Data,Typeable)
type Buckets = [Bucket]

data Context = Context
  { executionModule     :: TH.Module
  , location            :: TH.Loc
  } deriving (Show,Read,Eq,Data,Typeable)

data State = State
  { currentMops         :: [Bucket]
  , currentPackageDesc  :: GenericPackageDescription
  } deriving (Show,Read,Eq,Data,Typeable)

-- The hope for this implementation of History is a memoizing version accessor.
-- This implementation will leak memory in the case of used-once versions,
-- but I hope the leak to be of an acceptable magnitude.
newtype MopHistory = MopHistory
  { history :: Version -> IO (Buckets,GenericPackageDescription) }

data Log
  = Alert    String
  | Critical String
  | Error    String
  | Warning  String
  | Notify   String
  | Info     String
  | Debug    String
  deriving (Show,Ord,Eq,Data,Typeable)

newtype Mop a = Mop
  { runMop :: WriterT [Log] (ReaderT Context (StateT State TH.Q)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Context
           , MonadState  State
           , MonadWriter [Log]
           )

--------------------------------------------------------------------------------

mop :: TH.Q [TH.Dec] -> TH.Q [TH.Dec]
mop mentry = do
  entry <- mentry

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
    HSE.ParseOk m           -> do
      when (hasExpand entry) $ run
    HSE.ParseFailed loc str -> fail $
      "Could not parse module at " ++ show loc ++ "\nError:\n\t:" ++ str

run ::

--------------------------------------------------------------------------------
-- Primitives to trigger functionality in the mop preprocess phase; we'll scan
-- splice phrases beginning with mop for these triggers. For example:
--
-- 'mop expand' - placed in an algebra module to create a instruction set,
-- coalgebra, interpreter set, and pairings.

expandFunSplice :: TH.Dec
expandFunSplice = TH.FunD (TH.mkName "expand") []

expand :: TH.Q [TH.Dec]
expand = return [expandFunSplice]

hasExpand :: [TH.Dec] -> Bool
hasExpand = elem expandFunSplice

--------------------------------------------------------------------------------
-- Mop directory: locate, initialize, read, and write

mopDir = ".mop/"

locateMopDir d
  | null d || d == "/" = promptForNewMopDir
  | otherwise = do
     dc <- filterValidDirectories =<< getDirectoryContents d
     if mopDir `elem` dc
     then return (d </> mopDir)
     else locateMopDir (takeDirectory d)

createMopDir d = createDirectory (d </> mopDir)

promptForNewMopDir = do
  cd <- takeDirectory <$> findCabalFile
  cwd <- getCurrentDirectory
  putStrLn $ "Could not find .mop directory in or above " ++ cwd
  putStrLn $ "Create new .mop directory at " ++ cd ++ "? [Y/n]"
  ln <- getLine
  case map toLower ln of
    xs | "y" `isPrefixOf` xs -> createMopDir cd
       | otherwise           -> error "No mop history directory - exiting."

readMop = History historyAccessor
  where
    historyAccessor v = do
      findMopDir
