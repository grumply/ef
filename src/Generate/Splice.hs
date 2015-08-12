{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Generate.Splice where

import Generate.Monad
import Generate.Utils

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
  return $ pred
         (foldr (\(at,lr) st -> either
                    (\d -> if at < st then st - d else st)
                    (\i -> if at < st then st + i else st)
                    lr
                ) x ds)

place :: Pretty a => SrcLoc -> (SrcLoc -> a) -> Mop [String]
place sl f = splice sl (f sl)

splice :: Pretty a => SrcLoc -> a -> Mop [String]
splice (SrcLoc fn ln _) a = do
  let rendered = prettyPrint a
      rs = lines rendered
      count = length rs
  insertLinesInFile ln count rs fn
  return rs

spliceWith :: Pretty a => (String -> String) -> SrcLoc -> a -> Mop [String]
spliceWith alter (SrcLoc fn ln _) a = do
  let rendered = prettyPrint a
      altered = alter rendered
      as = lines altered
      count = length as
  insertLinesInFile ln count as fn
  return as

insertLinesInFile :: Int -> Int -> [String] -> FilePath -> Mop ()
insertLinesInFile at count ls fp = do
  off <- calculateOffset fp at
  io $ do
    cs <- lines <$> readFile fp
    cs `seq` do
      let cs' = unlines $ insertRange at ls cs
      length cs' `seq` writeFile fp cs'
  logInsert fp off count


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
  io (putStrLn $ "Unsplicing at " ++ show at ++ " in " ++ fp ++ " of "++ show count ++ " lines.")
  ls <- io $ do
    cs <- lines <$> readFile fp
    cs `seq` do
      let (ls,cs') = deleteRange off count cs
      length cs' `seq` writeFile fp $ unlines cs'
      return ls
  logDelete fp off count
  return ls
