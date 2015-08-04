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

import qualified Data.Aeson as JSON
import Data.Char
import Data.Data
import Data.Function
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Typeable

import GHC.Generics

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Language.Haskell.Exts as HSE
import           Language.Haskell.Exts.Syntax

import Derives
import qualified Product
import qualified Sum

import System.Exit
import System.IO
import System.Directory
import System.FilePath
import System.Posix.IO
import System.Posix.Files

import Distribution.ModuleName as Dist
import Distribution.Package
import Distribution.PackageDescription hiding (Var,Lit)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as Map

import Prelude hiding (log)

data BucketName = BucketName String
  deriving (Show,Read,Eq,Data,Typeable)

data BucketVersion = BucketVersion Version
  deriving (Show,Read,Eq,Data,Typeable)

data BucketConfiguration = BucketConfiguration
  { bucketName         :: BucketName
  , bucketVersion      :: BucketVersion
  } deriving (Show,Read,Eq,Data,Typeable)

data Bucket = Bucket
  { bucketConfig        :: BucketConfiguration
  , bucketAlgebras      :: [HSE.Module]
  , bucketCoalgebras    :: [HSE.Module]
  , bucketInstructions  :: [HSE.Module]
  , bucketInterpreters  :: [HSE.Module]
  , bucketPairings      :: [HSE.Module]
  , bucketOtherModules  :: [HSE.Module]
  } deriving (Show,Read,Eq,Data,Typeable)
type Buckets = [Bucket]

data MopContext = MopContext
  { executionModule     :: TH.Module
  , location            :: TH.Loc
  , originalModule      :: HSE.Module
  , verbosity           :: Verbosity
  } deriving (Show,Read,Eq,Data,Typeable)

-- would like to include MopHistory, but prevents data/typeable
-- will tuple it up in StateT s in Mop execution context.
data MopState = MopState
  { currentBuckets      :: [Bucket]
  , currentPackageDesc  :: GenericPackageDescription
  , cabalFile           :: FilePath
  , currentModule       :: HSE.Module
  , deletes             :: Map.Map FilePath [Int]
  } deriving (Show,Read,Eq,Data,Typeable)

data Log
  = Alert    String
  | Critical String
  | Error    String
  | Warning  String
  | Notify   String
  | Info     String
  | Debug    String
  deriving (Show,Ord,Eq,Data,Typeable)

data Verbosity
  = ReallySilent -- ^   Nothing
  | Silent       -- ^ < Warning; default
  | Quiet        -- ^ < Info
  | Normal       -- ^ < Debug
  | Loud         -- ^   All
  deriving (Read,Show,Eq,Ord,Enum,Data,Typeable)


data Algebra = Algebra
  { algebraName         :: HSE.Name
  , algebraType         :: HSE.Decl
  , algebraComponents   :: [HSE.Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data Coalgebra = Coalgebra
  { coalgebraName       :: HSE.Name
  , coalgebraType       :: HSE.Decl
  , coalgebraComponents :: [HSE.Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data Instructions = Instructions
  { instructions        :: [HSE.Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data Interpreters = Interpreters
  { interpreters        :: [HSE.Decl]
  } deriving (Show,Read,Eq,Data,Typeable)

data Pairings = Pairings
  { pairings            :: [(HSE.Decl,HSE.Decl,HSE.Decl)]
  } deriving (Show,Read,Eq,Data,Typeable)

newtype Mop a = Mop
  { runMop :: WriterT [Log] (ReaderT MopContext (StateT MopState TH.Q)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader MopContext
             , MonadState  MopState
             , MonadWriter [Log]
             )

liftTH :: TH.Q a -> Mop a
liftTH f = Mop $ lift $ lift $ lift f

io :: IO a -> Mop a
io = liftTH . TH.runIO

insertDelete :: FilePath -> Int -> Mop ()
insertDelete fp x = do
  ms@MopState{..} <- get
  put ms { deletes = Map.insertWith fp (x:) deletes }

calculateDeleteOffset :: FilePath -> Int -> Mop Int
calculateDeleteOffset fp x = do
  MopState _ _ _ _ (maybe [] reverse . Map.lookup fp -> ds) <- get
  return (foldr (\a st -> if a < st then pred st else st) x ds)

--------------------------------------------------------------------------------

mop :: Verbosity -> TH.Q [TH.Dec] -> TH.Q [TH.Dec]
mop vbosity mentry = do
  entry <- mentry

  TH.Module pn (TH.ModName mn) <- TH.thisModule
  TH.Loc{..}                   <- TH.location

  let l = loc_filename
      d = takeDirectory l

  (pm,c,cf) <- TH.runIO $ do
    pm      <- HSE.parseFile l

    cf      <- findCabalFile d
    c       <- readCabalFile cf

    return (pm,c,cf)

  case pm of
    HSE.ParseOk m           -> do
      TH.runIO (print m)
      if hasExpand entry
      then run (MopContext (TH.Module pn (TH.ModName mn)) TH.Loc{..} m vbosity)
               (MopState [] c cf m mempty)
               expandAlgebra
      else return []
    HSE.ParseFailed loc str -> fail $
      "Could not parse module at " ++ show loc ++ "\nError:\n\t:" ++ str

run :: MopContext -> MopState -> Mop a -> TH.Q a
run ctxt st f = do
  ((a,s),_) <-   flip runStateT  st
               . flip runReaderT ctxt
               .      runWriterT
               $      runMop     f
  TH.runIO (mapM_ print s)
  return a

expandAlgebra :: Mop [TH.Dec]
expandAlgebra = do
  alg <- createAlgebra
  writeCoalgebra    (createCoalgebra    alg)
  writeInstructions (createInstructions alg)
  writeInterpreters (createInterpreters alg)
  writePairings     (createPairings     alg)
  renderAlgebra alg

createAlgebra :: Mop Algebra
createAlgebra = do
  MopContext{..} <- ask
  let ds      = gatherDataDecls mn mx originalModule
      (nm,mn) = findExpand            originalModule
      mx      = findStop              originalModule
  if null ds
  then do
    log Error $ "No instructions found between lines "
                ++ show mn ++ " and " ++ show mx ++ "."
    io exitFailure
  else do
    log Notify "Algebraic components extracted."

    ty <- makeType nm ds

    unsplice mn
    unsplice mx

    return (Algebra (HSE.Ident nm) ty ds)
  where
    gatherDataDecls mn mx (Module _ _ _ _ _ _ (filter (p mn mx) -> xs)) = xs
    p mn mx (DataDecl (SrcLoc _ l _) _ _ _ _ _ _) = l > mn && l < mx
    p _  _  _                                     = False

    makeType nm ds = do
     loc <- asks location
     let getNameVars ~(DataDecl _ _ _ nm vs _ _) = (nm,vs)
         ds' = groupBy ((==) `on` (length . snd)) (map getNameVars ds)
     if length ds' == 1
     then return $ foldr (\st a -> ) (TypeDecl (SrcLoc loc_filename loc_start 0) (Name nm) [] (TyInfix _ (QName _) _)) ds
     else do
       log Error "Gathered data declarations have different number of type variable arguments."
       io exitFailure




renderAlgebra      :: Algebra -> Mop [TH.Dec]
renderAlgebra      = undefined

writeCoalgebra     :: Coalgebra -> Mop ()
writeCoalgebra     = undefined
writeInstructions  :: Instructions -> Mop ()
writeInstructions  = undefined
writeInterpreters  :: Interpreters -> Mop ()
writeInterpreters  = undefined
writePairings      :: Pairings -> Mop ()
writePairings      = undefined

createCoalgebra    :: Algebra -> Coalgebra
createCoalgebra    = undefined
createInstructions :: Algebra -> Instructions
createInstructions = undefined
createInterpreters :: Algebra -> Interpreters
createInterpreters = undefined
createPairings     :: Algebra -> Pairings
createPairings     = undefined

--------------------------------------------------------------------------------
-- Primitives to trigger functionality in the mop preprocess phase; we'll scan
-- splice phrases beginning with mop for these triggers. For example:
--
-- 'mop (expand "some_name")' - placed in an algebra module to create an
-- instruction set, coalgebra, interpreter set, and pairings. Will extract
-- algebraic components up to a 'stop' splice if one exists.


unsplice n = do
  TH.Loc{..} <- asks location
  x <- calculateDeleteOffset loc_filename n
  str <- deleteLine n loc_filename
  insertDelete loc_filename x
  return str

deleteLine :: Int -> FilePath -> Mop String
deleteLine n fp = io $ do
  h <- openFile fp ReadWriteMode
  cntnt <- lines <$> hGetContents h
  let begin = take (pred n) cntnt
      (toDelete:end) = drop (pred n) cntnt
  let cntnt' = unlines $ begin ++ end
  length cntnt' `seq` hPutStr h cntnt'
  hClose h
  return toDelete

expandFunSplice :: String -> TH.Dec
expandFunSplice str =
  TH.FunD
    (TH.mkName "expand")
    [TH.Clause
       []
       (TH.NormalB
          (TH.VarE (TH.mkName str))
       )
       []
    ]

expand :: String -> TH.Q [TH.Dec]
expand str = return [expandFunSplice str]

hasExpand :: [TH.Dec] -> Bool
hasExpand [] = False
hasExpand (TH.FunD (TH.nameBase -> "expand") _:_) = True
hasExpand (x:xs) = hasExpand xs

findExpand :: HSE.Module -> (String,Int)
findExpand (HSE.Module _ _ _ _ _ _ decls) =
  head $ mapMaybe getExpandSplice decls
  where
    getExpandSplice (HSE.SpliceDecl (SrcLoc _ l _) e) =
      case e of
        App (Var (UnQual (Ident "mop")))
            (Paren (App (Var (UnQual (Ident "expand")))
                        (Lit (String x))
                   )
            )        -> Just (x,l)
        _            -> Nothing
    getExpandSplice _ = Nothing

expandStopSplice :: TH.Dec
expandStopSplice = TH.FunD (TH.mkName "stop") []

stop :: TH.Q [TH.Dec]
stop = return [expandStopSplice]

findStop :: HSE.Module -> Int
findStop (HSE.Module _ _ _ _ _ _ decls) =
  let ss = mapMaybe getStopSplice decls
  in if null ss
     then maxBound
     else head ss
  where
    getStopSplice s@(HSE.SpliceDecl (SrcLoc _ l _) e) =
      case e of
        Var (UnQual (Ident "stop")) -> Just l
        _                           -> Nothing
    getStopSplice _                  = Nothing

--------------------------------------------------------------------------------
-- Mop helper functions for manipulating state, viewing environment and logging.

moduleDirectory :: String -> String
moduleDirectory = foldl1 (</>) . break [] []
  where
    break acc cur [] = reverse (reverse cur:acc)
    break acc cur ('.':xs) = break (reverse cur:acc) [] xs
    break acc cur (x:xs) = break acc (x:cur) xs

createModuleName x (HSE.ModuleName str) = HSE.ModuleName (x ++ '.':str)

createStandardDir dir srcDir (HSE.ModuleName x) = do
  let d = srcDir </> dir </> moduleDirectory x
  de <- doesDirectoryExist d
  unless de (createDirectory d)
  return d

createStandardModule dir srcDir mn@(HSE.ModuleName x) = do
  d <- createStandardDir dir srcDir mn
  let f = d </> dir <.> "hs"
  fe <- doesFileExist f
  unless fe (createFile f stdFileMode >>= closeFd)

log :: (String -> Log) -> String -> Mop ()
log x str = do
  when (x str < Info str) (io (print (x str)))
  tell [x str]

--------------------------------------------------------------------------------
-- Cabal utilities injected into the mop scope

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
readCabalFile f = readPackageDescription Verbosity.normal f

modifyOtherModules :: ([Dist.ModuleName] -> [Dist.ModuleName]) -> Mop ()
modifyOtherModules f = do
  MopState bs pkg fp hsem ds <- get
  let pd = packageDescription pkg
      Just lib@Library{..} = library pd
      bi@BuildInfo{..} = libBuildInfo
      oms = f otherModules
      lbi = bi { otherModules = oms }
      pkg' = pkg { packageDescription = pd { library = Just lib { libBuildInfo = lbi } } }
  put $ MopState bs pkg' fp hsem ds

modifyExposedModules :: ([Dist.ModuleName] -> [Dist.ModuleName]) -> Mop ()
modifyExposedModules f = do
  MopState bs pkg fp hsem ds <- get
  let pd = packageDescription pkg
      Just lib@Library{..} = library pd
      ems = f exposedModules
      pkg' = pkg { packageDescription = pd { library = Just lib { exposedModules = ems } } }
  put $ MopState bs pkg' fp hsem ds

addOtherModule m = modifyOtherModules (m:)
removeOtherModule m = modifyOtherModules (filter (/=m))

sourceDirectories :: Mop [FilePath]
sourceDirectories = do
  MopState bs pkg fp hsem ds <- get
  let pd = packageDescription pkg
      Just Library{..} = library pd
      BuildInfo{..} = libBuildInfo
  return hsSourceDirs

modifyGenericPackageDescription :: (GenericPackageDescription -> GenericPackageDescription) -> Mop ()
modifyGenericPackageDescription f =
  modify (\st -> st { currentPackageDesc = f $ currentPackageDesc st })

modifyPackageDescription :: (PackageDescription -> PackageDescription) -> Mop ()
modifyPackageDescription f =
  modifyGenericPackageDescription
    (\st -> st { packageDescription = f (packageDescription st) })

--------------------------------------------------------------------------------
-- HSE module modification utility functions for the module in state.

modifyModule :: (HSE.Module -> HSE.Module) -> Mop ()
modifyModule f =
  modify (\st -> st { currentModule = f $ currentModule st })

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

--------------------------------------------------------------------------------
-- Module creation utility functions

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

findModules ty = do
  sds <- sourceDirectories
  liftTH $ TH.runIO $ concat <$> mapM go sds
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
