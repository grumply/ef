-- | -- DO NOT USE --
-- This is abhorrent and disgusting Haskell. These dragons are mutated and move orthogonally; expect nothing predictable.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Data.List (intersperse,unfoldr)

import Data.Array.IO
import Data.Array.Base
import Data.Array.Unsafe
import Data.IORef
import System.IO.Unsafe
import Debug.Trace

import qualified Data.Heap as Heap

main' = do
    let
        heap = Heap.fromAscList [1..10000000] :: Heap.MinHeap Integer

    print =<< (extractAll heap)
    where
        extractAll =
            go 0 0
            where
                go !prev !n !heap =
                    case Heap.view heap of

                        Just (min,newHeap) -> 
                            if min > prev then
                                go min (n + 1) newHeap
                            else
                                do
                                    print $ show min ++ " /> " ++ show prev
                                    return 0
                        Nothing ->
                            return n

main = do
    currentSize <- newIORef 10000000
    maxSize <- newIORef 10000000
    minHeap <- newListArray (0,10000000) (undefined:[1..10000000 :: Integer])
    let

        heap =
            Heap currentSize maxSize minHeap
    
    print =<< extractAll heap
    where
        extractAll heap = 
            go 0 0
            where
                go !prev !n =
                    do
                        currentMin <- extractMin heap
                        case currentMin of

                            Nothing -> 
                                return n

                            Just min ->
                                if min > prev then
                                    go min (n + 1)
                                else 
                                    do
                                        print $ show min ++ " /> " ++ show prev
                                        return 0

data MinHeap a =
    Heap
        {
          currentSize
              :: {-# UNPACK #-} !(IORef Int)
              
        , maxSize
              :: {-# UNPACK #-} !(IORef Int)

        , minHeap
              :: {-# UNPACK #-} !(IOArray Int a)

        }



instance Show a => Show (MinHeap a)
    where

        show Heap{..} =
            "Heap"
                ++ "  {"
                       ++ "\n\t\tcurrentSize: " ++ show (unsafePerformIO $ readIORef currentSize)
                       ++ "\n\t\tmaxSize: " ++ show (unsafePerformIO $ readIORef maxSize)
                       ++ "\n\t\tminHeap: [" ++ concat (intersperse "," showMinHeap) ++ "]\n"
                ++ "  }"
            where

                showMinHeap =
                    go 1
                    where

                        go index
                            | index > (unsafePerformIO $ readIORef currentSize) =
                                  []
                            | otherwise =
                                  let
                                      valueAtIndex =
                                          unsafePerformIO $ readArray minHeap index

                                  in
                                      show valueAtIndex : go (index + 1)



type Height =
    Int



-- | sizeToHeight converts a binary tree size to its minimum height.
sizeToHeight
    :: Int
    -> Int

sizeToHeight =
    ceiling . logBase 2 . fromIntegral



-- | heightToSize converts a binary tree height to its maximum size.
heightToSize
    :: Int
    -> Int

heightToSize =
    pred . (2^)



-- | Create an empty min-heap.
empty
    :: Ord a
    => IO (MinHeap a)

empty =
    sizedEmpty 2



-- | Create an empty min-heap of the given height.
sizedEmpty
    :: Ord a
    => Height
    -> IO (MinHeap a)

sizedEmpty height =
    do
        let
            size =
                heightToSize height

        currentSize <- newIORef 0
        maxSize <- newIORef size
        minHeap <- newArray_ (0,size)
        return Heap{..}



-- | Create a new min-heap from the given size and list, unsafely.
new
    :: Ord a
    => [a]
    -> IO (MinHeap a)

new as =
    do
        let
            count =
                length as

            size =
                heightToSize (sizeToHeight count)

            elements =
                as

        currentSize <- newIORef count
        maxSize <- newIORef size
        minHeap <- newListArray (0,size) (undefined:elements)

        sort Heap{..}



isEmpty
    :: MinHeap a
    -> IO Bool

isEmpty Heap{..} =
    (== 0) <$> readIORef currentSize



isFull
    :: MinHeap a
    -> IO Bool

isFull Heap{..} =
    do
        currentSize <- readIORef currentSize
        maxSize <- readIORef maxSize
        return (currentSize == maxSize)



grow
    :: MinHeap a
    -> IO (MinHeap a)

grow Heap{..} =
    undefined




sort
    :: Ord a
    => MinHeap a
    -> IO (MinHeap a)

sort Heap {..} = 
    return Heap {..}



viewMin
    :: MinHeap a
    -> IO (Maybe a)

viewMin Heap{..} =
    do
        currentSize <- readIORef currentSize
        if currentSize == 0 then
            return Nothing
        else
            do
                min <- unsafeRead minHeap 1
                return (Just min)



extractMin
    :: (Show a, Ord a)
    => MinHeap a
    -> IO (Maybe a)

extractMin Heap{..} =
    do
        curSize <- readIORef currentSize
        case curSize of

            0 ->
                return Nothing

            1 ->
                do
                    min <- unsafeRead minHeap 1
                    writeIORef currentSize 0
                    return (Just min)

            _ ->
                do
                    min <- unsafeRead minHeap 1
                    largest <- unsafeRead minHeap curSize
                    unsafeWrite minHeap 1 largest
                    sink curSize largest minHeap
                    let
                        newCurSize =
                            curSize - 1
                    newCurSize `seq`
                        writeIORef currentSize newCurSize
                    return (Just min)

{-# INLINE sink #-}
sink
    :: Ord a
    => Int
    -> a
    -> IOArray Int a
    -> IO ()
    
sink curSize largest minHeap =
    go 1
    where
    
        {-# INLINE go #-}
        go !index =
            do
                let
                    leftIndex =
                        index * 2

                    hasLeft =
                        leftIndex <= curSize

                    rightIndex =
                        leftIndex + 1

                    hasRight =
                        rightIndex <= curSize

                    readLeft =
                        unsafeRead minHeap leftIndex

                    readRight =
                        unsafeRead minHeap rightIndex

                    current =
                        (largest,index)

                smaller@(small,smallIndex) <-
                    if hasLeft then
                        do
                            left <- readLeft
                            return $
                                if left < largest then
                                    (left,leftIndex)
                                else
                                    current

                    else
                        return current
                (smallest,smallestIndex) <-
                    if hasRight then
                        do
                            right <- readRight
                            return $
                                if right < small then
                                    (right,rightIndex)
                                else
                                    smaller
                    else
                        return smaller

                when (smallestIndex /= index) $
                    do
                        unsafeWrite minHeap smallestIndex largest
                        unsafeWrite minHeap index smallest
                        go smallestIndex





insert
    :: Ord a
    => a
    -> MinHeap a
    -> IO (MinHeap a)

insert new heap =
    do
        full <- isFull heap
        if full then
            undefined
        else
            undefined


bubble
    :: Ord a
    => Int
    -> IOArray Int a
    -> IO ()

bubble index arr =
    undefined


{-# INLINE heightToSize #-}
{-# INLINE sizeToHeight #-}
{-# INLINE extractMin #-}
{-# INLINE insert #-}
{-# INLINE bubble #-}
{-# INLINE new #-}
{-# INLINE viewMin #-}
{-# INLINE empty #-}
{-# INLINE sizedEmpty #-}
