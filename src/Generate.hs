{-# LANGUAGE RecordWildCards #-}
module Generate where

import Generate.Cabal
import Generate.DSL.Expand
import Generate.DSL.Project
import Generate.Monad

import Data.Function
import Data.List
import Data.Ord

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import System.IO
import System.FilePath

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
    c       <- readCabalFileIO cf

    return (pm,c,cf)
  case pm of
    ParseOk m           -> do
      run (MopContext (TH.Module pn (TH.ModName mn)) TH.Loc{..} m vbosity)
          (MopState c cf m mempty)
          $ case () of
              _ | hasExpand entry  -> expandSymbols    >> writeCabalFile
                | hasProject entry -> createProjection >> writeCabalFile
                | otherwise        -> return ()
      return []
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
