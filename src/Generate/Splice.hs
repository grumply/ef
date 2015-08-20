{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Generate.Splice where

import Generate.Monad
import Generate.Utils

import Control.Category

import Data.Char
import qualified Data.Map as Map
import Data.Maybe

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Ppr as TH

import Prelude hiding (log,id,(.))

import System.IO

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
  MopState _ _ _ (maybe [] reverse . Map.lookup fp -> ds) <- get
  return $ (foldr (\(at,lr) st ->
                     either
                       (\d -> if at < st then st - d else st)
                       (\i -> if at < st then st + i else st)
                       lr
                  )
                  x
                  ds
           )

place :: Pretty a => SrcLoc -> (SrcLoc -> a) -> Mop [String]
place sl f = splice sl (f sl)

placeWith :: Pretty a => SrcLoc -> (SrcLoc -> a) -> (String -> String) -> Mop [String]
placeWith sl f p = spliceWith p sl (f sl)

placeTH :: TH.Ppr a => SrcLoc -> (SrcLoc -> a) -> Mop [String]
placeTH sl f = spliceTH sl (f sl)

placeTHWith :: TH.Ppr a => SrcLoc -> (SrcLoc -> a) -> (String -> String) -> Mop [String]
placeTHWith sl f p = spliceTHWith p sl (f sl)

splice :: Pretty a => SrcLoc -> a -> Mop [String]
splice = spliceWith id

spliceTH :: TH.Ppr a => SrcLoc -> a -> Mop [String]
spliceTH = spliceTHWith id

spliceInReverseOrder :: Pretty a => SrcLoc -> [a] -> Mop [String]
spliceInReverseOrder sl splicable
  = concat <$> sequence (foldr ((>>>) . (:) . splice sl) id splicable [])

spliceTHInReverseOrder :: TH.Ppr a => SrcLoc -> [a] -> Mop [String]
spliceTHInReverseOrder sl splicable
  = concat <$> sequence (foldr ((>>>) . (:) . spliceTH sl) id splicable [])

spliceAtEnd :: Pretty a => SrcLoc -> a -> Mop [String]
spliceAtEnd sl = spliceWith id sl { srcLine = maxBound }

spliceAtEndWith :: Pretty a => SrcLoc -> (String -> String) -> a -> Mop [String]
spliceAtEndWith sl p = spliceWith p sl { srcLine = maxBound }

spliceTHAtEnd :: TH.Ppr a => SrcLoc -> a -> Mop [String]
spliceTHAtEnd sl = spliceTHWith id sl { srcLine = maxBound }

spliceTHAtEndWith :: TH.Ppr a => SrcLoc -> (String -> String) -> a -> Mop [String]
spliceTHAtEndWith sl p = spliceTHWith p sl { srcLine = maxBound }

spliceInReverseOrderAtEnd :: Pretty a => SrcLoc -> [a] -> Mop [String]
spliceInReverseOrderAtEnd sl = spliceInReverseOrder sl { srcLine = maxBound }

spliceTHInReverseOrderAtEnd :: TH.Ppr a => SrcLoc -> [a] -> Mop [String]
spliceTHInReverseOrderAtEnd sl = spliceTHInReverseOrder sl { srcLine = maxBound }

spliceWith :: Pretty a => (String -> String) -> SrcLoc -> a -> Mop [String]
spliceWith alter sl@(SrcLoc fn ln _) a = do
  let rendered = prettyPrint a
      altered = alter rendered
      dropEmpty = dropWhile (\x -> null x || all isSpace x)
      trimLines = reverse . dropEmpty . reverse . dropEmpty
      as = trimLines $ lines altered
      count = length as
  off <- calculateOffset fn ln
  io $ do
    cs <- lines <$> readFile fn
    cs `seq` do
      let cs' = insertRange off as cs
          lcs' = length cs'
      lcs' `seq` writeFile fn $ unlines $
        if lcs' == length cs then cs' ++ as else cs'

  logInsert fn off count
  log Notify ("Generate.Splice.splice: " ++ show sl ++ " (" ++ show ln ++ " => " ++ show off ++ "):\n\t" ++ unlines as)
  return as

spliceTHWith :: TH.Ppr a => (String -> String) -> SrcLoc -> a -> Mop [String]
spliceTHWith alter sl@(SrcLoc fn ln _) a = do
  let rendered = TH.pprint a
      altered = alter rendered
      dropEmpty = dropWhile (\x -> null x || all isSpace x)
      trimLines = reverse . dropEmpty . reverse . dropEmpty
      as = trimLines $ lines altered
      count = length as
  off <- calculateOffset fn ln
  io $ do
    cs <- lines <$> readFile fn
    cs `seq` do
      let cs' = insertRange off as cs
          lcs' = length cs'
      lcs' `seq` writeFile fn $ unlines $
        if lcs' == length cs then cs' ++ as else cs'

  logInsert fn off count
  log Notify ("Generate.Splice.spliceTH: " ++ show sl ++ " (" ++ show ln ++ " => " ++ show off ++ "):\n\t" ++ unlines as)
  return as

deleteLine :: Int -> FilePath -> Mop String
deleteLine at fp = do
  strs <- unsplice at 1 fp
  case strs of
    (x:_) -> return x
    [] -> do log Warning $ "Generate.Splice.deleteLine from "
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
  log Notify ("Generate.Splice.unsplice: " ++ show (SrcLoc fp at 1) ++ " (" ++ show at ++ " => " ++ show off ++ "):\n\t" ++ unlines ls)
  return ls

nextLine :: SrcLoc -> SrcLoc
nextLine (SrcLoc fp l c) = SrcLoc fp (succ l) c
