{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
-- module Ef.Data.MinHeap where
module Main where

import Control.Monad
import Control.Monad.ST

import Data.Array.ST
import Data.STRef



data MinHeap s a
    where

        Heap
            :: STRef s Int
            -> STArray s Int a
            -> MinHeap s a


main = do
    let
        min =
            runST $
                do
                    array <- newListArray (1,5000000) [1 .. 5000000 :: Int]
                    minHeap <- buildMinHeap array
                    replicateM 10 (extractMinST minHeap)
    print min
    -- print $ head $ reverse [5000000,49999995000,4999 .. 1 .. 1 :: Int]

buildMinHeap
    :: Ord a
    => STArray s Int a
    -> ST s (MinHeap s a)

buildMinHeap array =
    do
        (_,elements) <- getBounds array
        elems <- newSTRef elements
        let
            largestIndex =
                elements `div` 2

            nextLargestIndex =
                pred largestIndex

        forM [largestIndex, nextLargestIndex .. 1] $ \index ->
            minHeapify (Heap elems array) index
        return (Heap elems array)



extractMinST
    :: Ord a
    => MinHeap s a
    -> ST s (Maybe a)

extractMinST (Heap elems heap) =
    do
        elements <- readSTRef elems
        if elements < 1 then
            return Nothing
        else
            do
                min <- readArray heap 1
                newFirst <- readArray heap elements
                writeArray heap 1 newFirst
                writeSTRef elems (pred elements)
                minHeapify (Heap elems heap) 1
                return (Just min)



minHeapify
    :: Ord a
    => MinHeap s a
    -> Int
    -> ST s ()

minHeapify (Heap elems heap) start =
    do
        current <- readArray heap start
        go start current
    where
        {-# INLINE go #-}
        go index current =
            do
                elements <- readSTRef elems
                let
                    leftIndex =
                        2 * index

                    rightIndex =
                        leftIndex + 1

                    hasLeft =
                        leftIndex <= elements

                    hasRight =
                        rightIndex <= elements

                    readLeft =
                        readArray heap leftIndex

                    readRight =
                        readArray heap rightIndex

                small@(smaller,smallerIndex) <-
                    if hasLeft then
                        do
                            left <- readLeft
                            return $
                                if left < current then
                                    (left,leftIndex)
                                else
                                    (current,index)

                    else
                        return (current,index)

                (smallest,indexOfSmallest) <-
                    if hasRight then
                        do
                            right <- readRight
                            return $
                                if right < smaller then
                                    (right,rightIndex)
                                else
                                    small
                    else
                        return small

                when (indexOfSmallest /= index) $
                    do
                        writeArray heap index smallest
                        writeArray heap indexOfSmallest current
                        go indexOfSmallest smallest



-- insert
--     :: Int
--     -> 

{-# INLINE extractMinST #-}
{-# INLINE minHeapify #-}
{-# INLINE buildMinHeap #-}
