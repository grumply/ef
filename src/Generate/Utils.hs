{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Generate.Utils where

import Generate.Monad

import Calypso.Static as C

import Control.Arrow
import Control.Monad

import Data.List

import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.IO

import Language.Haskell.Exts

class TranslationalEq a b where
  (~==) :: a -> b -> Bool

-- instance TranslationalEq a b => TranslationalEq (SrcLoc -> a) (SrcLoc -> b) where
--   (~==) x y = (x undefined) ~== (y undefined)

instance {-# OVERLAPPABLE #-} TranslationalEq a b => TranslationalEq (SrcLoc -> a) b where
  (~==) x y = (x undefined) ~== y

instance {-# OVERLAPPABLE #-} TranslationalEq a b => TranslationalEq a (SrcLoc -> b) where
  (~==) x y = x ~== (y undefined)

instance {-# OVERLAPPABLE #-} TranslationalEq a b => TranslationalEq b a where
  (~==) a b = b ~== a

class Locatable a where
  locate :: a -> SrcLoc
  relocate :: SrcLoc -> a -> a

lineAfter :: SrcLoc -> SrcLoc
lineAfter (SrcLoc fp l c) = SrcLoc fp (succ l) c

-- | 0-indexed deletion of a given number of elements `count` starting at `at`.
--
-- deleteRange 3 3 [1..9] == ([4,5,6],[1,2,3,7,8,9])
deleteRange :: Int -> Int -> [a] -> ([a],[a])
deleteRange at count = first (either (const []) reverse . snd)
                     . C.accum go (count,Left 1)
  where
    go = do
      (i,a) <- C.view
      case i of
        (0,_) -> C.yield a
        (n,Right ds) -> C.put (n-1,Right (a:ds))
        (n,Left l) -> do
          C.yield a
          C.put (n,if l == at then Right [] else Left (succ l))

-- | 1-indexed splicing
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

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

moduleDirectory :: String -> String
moduleDirectory = foldl1 (</>) . break [] []
  where
    break acc cur [] = reverse (reverse cur:acc)
    break acc cur ('.':xs) = break (reverse cur:acc) [] xs
    break acc cur (x:xs) = break acc (x:cur) xs

createModuleName x (ModuleName str) = ModuleName (x ++ '.':str)

createStandardDir dir srcDir (ModuleName x) = do
  let d = srcDir </> dir </> moduleDirectory x
  de <- doesDirectoryExist d
  unless de (createDirectory d)
  return d

createStandardModule dir srcDir mn@(ModuleName x) = do
  d <- createStandardDir dir srcDir mn
  let f = d </> dir <.> "hs"
  fe <- doesFileExist f
  unless fe (createFile f stdFileMode >>= closeFd)

mopCreateDir dir srcDir (ModuleName x) = do
  let d = srcDir </> dir </> moduleDirectory x
  de <- doesDirectoryExist d
  unless de (createDirectory d)
  return d

mopCreateModule dir srcDir mn@(ModuleName x) = do
  d <- mopCreateDir dir srcDir mn
  let f = d </> dir <.> "hs"
  fe <- doesFileExist f
  unless fe (createFile f stdFileMode >>= closeFd)

findModules ty = do
  sds <- sourceDirectories
  io $ concat <$> mapM go sds
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
