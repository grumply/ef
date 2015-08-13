{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Generate.Splice (splice,unsplice,deleteLine,place,placeWith,spliceWith,nextLine) where

import Generate.Monad
import Generate.Utils

import Data.Char
import qualified Data.Map as Map
import Data.Maybe

import Prelude hiding (log)

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
                       lr -- might need to change to (at < st)
                  )
                  x
                  ds
           )

place :: Pretty a => SrcLoc -> (SrcLoc -> a) -> Mop [String]
place sl f = splice sl (f sl)

placeWith :: Pretty a => SrcLoc -> (SrcLoc -> a) -> (String -> String) -> Mop [String]
placeWith sl f p = spliceWith p sl (f sl)

splice :: Pretty a => SrcLoc -> a -> Mop [String]
splice = spliceWith id

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
      lcs' `seq`
        if lcs' == length cs
         then writeFile fn $ unlines $ cs' ++ as
         else writeFile fn $ unlines cs'

  logInsert fn off count
  log Notify ("Generate.Splice.splice: " ++ show sl ++ " (" ++ show ln ++ " => " ++ show off ++ "):\n\t" ++ unlines as)
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
