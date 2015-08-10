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
  (mop,expand,stop,Verbosity(..))
  where

import qualified Calypso.Static as C

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
  , changes             :: Map.Map FilePath [(Int,Either Int Int)]
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

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

logDelete :: FilePath -> Int -> Int -> Mop ()
logDelete fp atLine deleted = do
  ms@MopState{..} <- get
  put ms { changes = Map.insertWith (++) fp [(atLine,Left deleted)] changes }

logInsert :: FilePath -> Int -> Int -> Mop ()
logInsert fp atLine inserted = do
  ms@MopState{..} <- get
  put ms { changes = Map.insertWith (++) fp [(atLine,Right inserted)] changes }

calculateOffset :: FilePath -> Int -> Mop Int
calculateOffset fp x = do
  MopState _ _ _ _ (maybe [] reverse . Map.lookup fp -> ds) <- get
  return (foldr (\(at,lr) st -> either
                    (\d -> if at < st then st + d else st)
                    (\i -> if at < st then st - i else st)
                    lr
                ) x ds)

spliceWith :: HSE.Pretty a => (String -> String) -> SrcLoc -> a -> Mop [String]
spliceWith alter (SrcLoc fn ln _) a = do
  let rendered = HSE.prettyPrint a
      altered = alter rendered
      as = lines altered
      count = length as
  insertLinesInFile ln count as fn
  return as

splice :: HSE.Pretty a => SrcLoc -> a -> Mop [String]
splice (SrcLoc fn ln _) a = do
  let rendered = HSE.prettyPrint a
      rs = lines rendered ++ [""]
      count = length rs
  insertLinesInFile ln count rs fn
  return rs


insertLinesInFile :: Int -> Int -> [String] -> FilePath -> Mop ()
insertLinesInFile at count ls fp = do
  off <- calculateOffset fp at
  io $ do
    cs <- lines <$> readFile fp
    cs `seq` do
      let cs' = unlines $ insertRange at ls cs
      length cs' `seq` writeFile fp cs'
  logInsert fp off count

insertRange :: Int -> [a] -> [a] -> [a]
insertRange _ [] = id
insertRange at ins = C.eval go 0
  where
    go = do
      (i,a) <- C.view
      case compare i at of
        LT -> C.put (succ i) >> C.yield a
        GT -> C.yield a
        EQ -> C.yields ins >> C.put (succ i) >> C.yield a

deleteLine :: Int -> FilePath -> Mop String
deleteLine at fp = do
  strs <- unsplice at 1 fp
  case strs of
    (x:_) -> return x
    [] -> do log Warning $ "Generate.deleteLine from "
                           ++ fp ++ " at " ++ show at
                           ++ ": File too short."
             return ""

unsplice :: Int -> Int -> FilePath -> Mop [String]
unsplice at count fp = do
  off <- calculateOffset fp at
  ls <- io $ do
    cs <- lines <$> readFile fp
    cs `seq` do
      let (ls,cs') = deleteRange off count cs
      length cs' `seq` writeFile fp $ unlines cs'
      return ls
  logDelete fp off count
  return ls

deleteRange :: Int -> Int -> [a] -> ([a],[a])
deleteRange at count = first (either (const []) reverse . snd) . C.accum go (count,Left 1)
  where
    go = do
      (i,a) <- C.view
      case i of
        (0,_) -> C.yield a
        (n,Right ds) -> C.put (n-1,Right (a:ds))
        (n,Left l) -> let l' = succ l in
          if l' == at
          then C.put (n,Right [])
          else C.yield a >> C.put (n,Left l')

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
  TH.runIO (mapM_ print [ x | x <- s, x > Info ""])
  return a

expandAlgebra :: Mop [TH.Dec]
expandAlgebra = do
  (slc,alg) <- createAlgebra
  spliceWith (filter (not . flip elem "`()")) slc (algebraType alg)
  -- writeCoalgebra    (createCoalgebra    alg)
  -- writeInstructions (createInstructions alg)
  -- writeInterpreters (createInterpreters alg)
  -- writePairings     (createPairings     alg)
  -- renderAlgebra alg
  return []

-- create an algebraic type from a series of functorial instructions.
-- This method assumes that the variables are similarly named. Bypassing
-- this method will be common in the case of a highly variable free variable
-- configuration.
createAlgebra :: Mop (SrcLoc,Algebra)
createAlgebra = do
  MopContext{..} <- ask
  let f = TH.loc_filename location
      m@(Module _ _ _ _ _ _ decls) = originalModule
  (algebraName,start) <- findExpand m
  let stop = findStop m
      instructions = [ x | x@(DataDecl (SrcLoc _ l _) _ _ _ _ _ _) <- decls
                         , l > start, l < stop ]
      namesAndVars = [ (n,vs) | DataDecl _ _ _ n vs _ _ <- instructions ]
      tyVars = mergeTypeVars $ gatherTypeVars instructions
      lrty = buildInstructionsType location start algebraName instructions
  if null instructions
  then errorAt "Could not find instructions" start stop
  else do
    l <- deleteLine start f
    log Notify ("Unspliced expand (line " ++ show start ++ "): " ++ show l)
    l' <- deleteLine stop f
    log Notify ("Unspliced stop (line" ++  show stop ++ "): " ++ show l)

    either
      (\str -> errorAt str start stop >> io exitFailure)
      (\ty@(TypeDecl s _ _ _) -> return $
          (s,Algebra (Ident algebraName) ty instructions)
      )
      lrty

errorAt :: String -> Int -> Int -> Mop a
errorAt err beg end = do
  log Error $
    err ++ " between lines " ++ show beg ++ " and " ++ show end ++ "."
  io exitFailure

buildInstructionsType :: TH.Loc -> Int -> String -> [Decl] -> Either String Decl
buildInstructionsType TH.Loc{..} srcLoc nm ds =
  let c = mergeContexts     $ gatherContexts     ds
      t = mergeTypeVars     $ gatherTypeVars     ds
      a = mergeInstructions $ gatherInstructions ds
      s = SrcLoc loc_filename srcLoc 0
      n = Ident nm
      y = TyForall Nothing c
  in either Left (\ty -> Right (TypeDecl s n t (y ty))) a

gatherContexts :: [Decl] -> [Context]
gatherContexts ds = [ c | (DataDecl _ _ c _ _ _ _) <- ds ]

mergeContexts :: [Context] -> Context
mergeContexts = nub . concat

gatherTypeVars :: [Decl] -> [[TyVarBind]]
gatherTypeVars ds = [ tyvs | (DataDecl _ _ _ _ (safeInit -> tyvs) _ _) <- ds ]

mergeTypeVars :: [[TyVarBind]] -> [TyVarBind]
mergeTypeVars = nub . concat

gatherInstructions :: [Decl] -> [(Name,[TyVarBind])]
gatherInstructions ds = [ (nm,tyvs) | (DataDecl _ _ _ nm tyvs _ _) <- ds ]

mergeInstructions :: [(Name,[TyVarBind])] -> Either String Type
mergeInstructions [] = Left "Generate.mergeInstructions: empty list"
mergeInstructions (d:ds) =
  let combine nmtys cont fixr = cont $ fix $ fixr $ uncurry build nmtys
      start = fix (uncurry build d)
      fix l r = TyInfix l (UnQual (Ident ":+:")) r
      build nm [] = error $ show nm ++ " has no free variables; not a functor."
      build nm (safeInit -> []) = TyCon (UnQual nm)
      build nm (safeInit -> tyvs) =
        TyCon (UnQual nm) `TyApp` (foldl1 TyApp $ map makeVar tyvs)
      makeVar (KindedVar nm _) = TyVar nm
      makeVar (UnkindedVar nm) = TyVar nm
      finish f =
        case f undefined of
          TyInfix l _ _ -> l
  in Right $ foldr combine finish ds start


-- renderAlgebra      :: Algebra -> Mop [TH.Dec]
-- renderAlgebra      = undefined

-- writeCoalgebra     :: Coalgebra -> Mop ()
-- writeCoalgebra     = undefined
-- writeInstructions  :: Instructions -> Mop ()
-- writeInstructions  = undefined
-- writeInterpreters  :: Interpreters -> Mop ()
-- writeInterpreters  = undefined
-- writePairings      :: Pairings -> Mop ()
-- writePairings      = undefined

-- createCoalgebra    :: Algebra -> Coalgebra
-- createCoalgebra    = undefined
-- createInstructions :: Algebra -> Instructions
-- createInstructions = undefined
-- createInterpreters :: Algebra -> Interpreters
-- createInterpreters = undefined
-- createPairings     :: Algebra -> Pairings
-- createPairings     = undefined

--------------------------------------------------------------------------------
-- Primitives to trigger functionality in the mop preprocess phase; we'll scan
-- splice phrases beginning with mop for these triggers. For example:
--
-- 'mop (expand "some_name")' - placed in an algebra module to create an
-- instruction set, coalgebra, interpreter set, and pairings. Will extract
-- algebraic components up to a 'stop' splice if one exists.



--------------------------------------------------------------------------------
-- DSL

expand :: String -> TH.Q [TH.Dec]
expand str = return [expandFunSplice str]

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

stop :: TH.Q [TH.Dec]
stop = return [expandStopSplice]

expandStopSplice :: TH.Dec
expandStopSplice = TH.FunD (TH.mkName "stop") []


--------------------------------------------------------------------------------
-- Helpers for DSL

hasExpand :: [TH.Dec] -> Bool
hasExpand [] = False
hasExpand (TH.FunD (TH.nameBase -> "expand") _:_) = True
hasExpand (x:xs) = hasExpand xs

findExpand :: HSE.Module -> Mop (String,Int)
findExpand (HSE.Module _ _ _ _ _ _ decls) =
  case mapMaybe getExpandSplice decls of
    (x:_) -> return x
    _ -> do log Error "No expand splice found in module decls."
            io exitFailure
  where
    getExpandSplice (HSE.SpliceDecl (SrcLoc _ l _) e) =
      case e of
        App (App (Var (UnQual (Ident "mop")))
                  (Con (UnQual (Ident _)))
             )
             (Paren (App (Var (UnQual (Ident "expand")))
                         (Lit (String x))
                    )
             )       -> Just (x,l)
        _            -> Nothing
    getExpandSplice _ = Nothing

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
