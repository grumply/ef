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
module Generate
  -- (mop,expand,stop,Verbosity(..)
  -- ,deleteRange, insertRange

  -- )
  where

import qualified Calypso.Static as C

import Generate.Cabal
import Generate.Derives
import Generate.DSL
import Generate.DSL.Expand
import Generate.DSL.Helpers
import Generate.Module
import Generate.Monad
import Generate.Representation
import Generate.Splice
import Generate.Splice.Import
import Generate.Utils

import Control.Applicative
import Control.Arrow
import Control.Monad

import qualified Data.Aeson as JSON
import Data.Char
import Data.Data
import Data.Either
import Data.Function
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Typeable

import Debug.Trace

import GHC.Generics

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Product
import qualified Sum

import System.Exit
import System.IO
import System.Directory
import System.FilePath
import System.Posix.IO
import System.Posix.Files

import qualified Data.Map as Map

import Prelude hiding (log)

mop :: Verbosity -> TH.Q [TH.Dec] -> TH.Q [TH.Dec]
mop vbosity mentry = do
  entry <- mentry

  TH.Module pn (TH.ModName mn) <- TH.thisModule
  TH.Loc{..}                   <- TH.location

  let l = loc_filename
      d = takeDirectory l

  (pm,c,cf) <- TH.runIO $ do
    pm      <- parseFile l

    cf      <- findCabalFile d
    c       <- readCabalFile cf

    return (pm,c,cf)

  case pm of
    ParseOk m           -> do
      if hasExpand entry
      then run (MopContext (TH.Module pn (TH.ModName mn)) TH.Loc{..} m vbosity)
               (MopState c cf m mempty)
               expandSymbols
      else return []
    ParseFailed loc str -> fail $
      "Mop.Generate: Could not parse module at " ++ show loc ++ "\nError:\n\t" ++ str

run :: MopContext -> MopState -> Mop a -> TH.Q a
run ctxt st f = do
  ((a,s),_) <-   flip runStateT  st
               . flip runReaderT ctxt
               .      runWriterT
               $      runMop     f
  TH.runIO (mapM_ print [ x | x <- s, x > Info ""])
  return a
