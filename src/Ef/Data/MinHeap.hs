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

import System.Random

main' = do
    let
        heap = Heap.fromList [1..10000000] :: Heap.MinHeap Integer

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

-- main = do
--     currentSize <- newIORef 10000000
--     maxSize <- newIORef 10000000
--     heap <- newListArray (0,10000000) (undefined:[1..10000000 :: Integer])
--     minHeap <- newIORef heap
--     let

--         heap =
--             Heap currentSize maxSize minHeap
    
--     print =<< extractAll heap
--     where
--         extractAll heap = 
--             go 0 0
--             where
--                 go !prev !n =
--                     do
--                         currentMin <- extractMin heap
--                         case currentMin of

--                             Nothing -> 
--                                 return n

--                             Just min ->
--                                 if min > prev then
--                                     go min (n + 1)
--                                 else 
--                                     do
--                                         print $ show min ++ " /> " ++ show prev
--                                         return 0

main =
    do
        gen <- newStdGen
        let
            rs =
                take 1000000 (randoms gen)

            heap =
                Heap.fromList rs :: Heap.MinHeap Int

        heap `seq` print "Done"

main'' = 
    do
        gen <- newStdGen
        let
            rs =
                take 1000000 (randoms gen)
        heap <- new (rs :: [Int])
        heap `seq` print "Done"

data MinHeap a =
    Heap
        {
          currentSize
              :: {-# UNPACK #-} !(IORef Int)
              
        , maxSize
              :: {-# UNPACK #-} !(IORef Int)

        , minHeap
              :: {-# UNPACK #-} !(IORef (IOArray Int a))

        }



instance Show a => Show (MinHeap a)
    where

        show Heap{..} =
            "Heap"
                ++ "  {"
                       ++ "\n\t\tcurrentSize: " ++ show (unsafePerformIO $ readIORef currentSize)
                       ++ "\n\t\tmaxSize: " ++ show (unsafePerformIO $ readIORef maxSize)
                       ++ "\n\t\tminHeap: [" ++ concat (intersperse "," (showMinHeap (unsafePerformIO $ readIORef minHeap))) ++ "]\n"
                ++ "  }"
            where

                showMinHeap heap =
                    go 1
                    where

                        go index
                            | index > (unsafePerformIO $ readIORef currentSize) =
                                  []
                            | otherwise =
                                  let
                                      valueAtIndex =
                                          unsafePerformIO $ readArray heap index

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

        heap <- newArray_ (0,size)
        currentSize <- newIORef 0
        maxSize <- newIORef size
        minHeap <- newIORef heap
        
        return Heap{..}



-- | Create a new min-heap from the given size and list, unsafely.
new
    :: (Ord a,Show a)
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

        heap <- newListArray (0,size) (undefined:elements)
        currentSize <- newIORef count
        maxSize <- newIORef size
        minHeap <- newIORef heap
        
        sort Heap{..}
        return Heap {..}



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
    :: Int
    -> MinHeap a
    -> IO ()

grow curSize Heap{..} =
    undefined




sort
    :: (Ord a,Show a)
    => MinHeap a
    -> IO ()

sort Heap {..} = 
    do
        curSize <- readIORef currentSize
        heap <- readIORef minHeap
        let
            middle =
                curSize `div` 2
                
            loopRange =
                [middle,middle - 1 .. 1]
        forM_ loopRange (sink Heap{..})



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
                heap <- readIORef minHeap
                min <- unsafeRead heap 1
                return (Just min)



extractMin
    :: (Show a, Ord a)
    => MinHeap a
    -> IO (Maybe a)

extractMin Heap{..} =
    do
        heap <- readIORef minHeap
        curSize <- readIORef currentSize
        if curSize == 0 then
            return Nothing
        else
            do
                min <- unsafeRead heap 1
                largest <- unsafeRead heap curSize
                unsafeWrite heap 1 largest
                sink Heap{..} 1
                let
                    newCurSize =
                        curSize - 1
                newCurSize `seq`
                    writeIORef currentSize newCurSize
                return (Just min)



{-# INLINE sink #-}
sink
    :: Ord a
    => MinHeap a
    -> Int
    -> IO ()

sink Heap{..} index0 =
    do
        curSize <- readIORef currentSize
        heap <- readIORef minHeap
        withCurrentSize curSize heap index0
    where
    
        withCurrentSize curSize heap =
            go 
            where
                
                {-# INLINE go #-}
                go !index =
                    do
                        sinking <- unsafeRead heap index
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
                                unsafeRead heap leftIndex

                            readRight =
                                unsafeRead heap rightIndex

                            current =
                                (sinking,index)

                        smaller@(small,smallIndex) <-
                            if hasLeft then
                                do
                                    left <- readLeft
                                    if left < sinking then
                                        return (left,leftIndex)
                                    else
                                        return current

                            else
                                return current

                        (smallest,smallestIndex) <-
                            if hasRight then
                                do
                                    right <- readRight
                                    if right < small then
                                        return (right,rightIndex)
                                    else
                                        return smaller
                            else
                                return smaller

                        when (smallestIndex /= index) $
                            do
                                unsafeWrite heap smallestIndex sinking
                                unsafeWrite heap index smallest
                                go smallestIndex





insert
    :: Ord a
    => a
    -> MinHeap a
    -> IO ()

insert new Heap{..} =
    do
        max <- readIORef maxSize
        curSize <- readIORef currentSize
        when (curSize == max) (grow curSize Heap{..})
        heap <- readIORef minHeap
        let
            !newCurrentSize =
                curSize + 1
        writeIORef currentSize newCurrentSize
        unsafeWrite heap newCurrentSize new
        bubble new heap newCurrentSize

bubble
    :: Ord a
    => a
    -> IOArray Int a
    -> Int
    -> IO ()

bubble new arr =
    go
    where
        go index =
            let
                parent =
                    index `div` 2

            in
                if parent > 0 then
                    do
                        parentValue <- unsafeRead arr parent
                        if parentValue > new then
                            do
                                unsafeWrite arr parent new
                                unsafeWrite arr index parentValue
                                go parent
                        else
                            return ()
                else 
                    return ()


{-# INLINE heightToSize #-}
{-# INLINE sizeToHeight #-}
{-# INLINE extractMin #-}
{-# INLINE insert #-}
{-# INLINE bubble #-}
{-# INLINE new #-}
{-# INLINE viewMin #-}
{-# INLINE empty #-}
{-# INLINE sizedEmpty #-}
