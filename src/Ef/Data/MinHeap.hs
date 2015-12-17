{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Control.Monad.ST

import Data.List (intersperse)

import GHC.Arr


main = do
    let
        minsorted =
            [undefined,1,2,3,2,3,6,7,2,8,4]

        arr =
            listArray (0,0) []

        minHeap =
            Heap 10 10 arr

        go 0 _ =
           []

        go n mh =
            let
                Just (min,newMH) =
                    extractMin mh

            in
                min:go (n - 1) newMH

    arr `seq` print (numElements arr) -- print (go 10 minHeap)



data MinHeap a =
    Heap
        {
          currentSize
              :: {-# UNPACK #-} !Int

        , maxSize
              :: {-# UNPACK #-} !Int

        , minHeap
              :: {-# UNPACK #-} !(Array Int a)

        }



instance Show a => Show (MinHeap a)
    where

        show (Heap currentSize maxSize minHeap) =
            "Heap"
                ++ "  {"
                       ++ "\n\t\tcurrentSize: " ++ show currentSize
                       ++ "\n\t\tmaxSize: " ++ show maxSize
                       ++ "\n\t\tminHeap: [" ++ concat (intersperse "," showMinHeap) ++ "]\n"
                ++ "  }"
            where

                showMinHeap =
                    go 1
                    where

                        go index
                            | index > currentSize =
                                  []
                            | otherwise =
                                  let
                                      valueAtIndex =
                                          unsafeAt minHeap index

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
    => MinHeap a

empty =
    sizedEmpty 2



-- | Create an empty min-heap of the given height.
sizedEmpty
    :: Ord a
    => Height
    -> MinHeap a

sizedEmpty (heightToSize -> maxSize) =
    let
        currentSize = 
            0
            
        minHeap =
            listArray (0,maxSize) []

    in
        Heap{..}



-- | Create a new min-heap from the given size and list. 
new
    :: Ord a
    => [a]
    -> MinHeap a
    
new as =
    let
        currentSize =
            length as

        maxSize =
            heightToSize (sizeToHeight currentSize)

        elements =
            zip [1..currentSize] as

        minHeap =
            unsafeArray' (0,maxSize) (succ maxSize) elements

    in
        sort Heap{..}



isEmpty
    :: Ord a
    => MinHeap a
    -> Bool

isEmpty Heap{..} =
    currentSize == 0



sort
    :: Ord a
    => MinHeap a
    -> MinHeap a

sort Heap{..} = Heap {..}
    -- runST $
    --     do
    --         undefined



viewMin
    :: Ord a
    => MinHeap a
    -> Maybe a
    
viewMin Heap{..} =
    if currentSize == 0 then
        Nothing
    else
        Just (unsafeAt minHeap 1)



extractMin
    :: forall a.
       (Ord a,Show a)
    => MinHeap a
    -> Maybe (a,MinHeap a)

extractMin Heap{..} =
    --trace (show $ Heap {..}) $
    case currentSize of

        0 ->
            Nothing

        1 ->
            runST $
                do
                    arr <- unsafeThawSTArray minHeap
                    min <- unsafeReadSTArray arr 1
                    newMinHeap <- unsafeFreezeSTArray arr
                    let
                        newHeap =
                            Heap 0 maxSize newMinHeap

                        result =
                            Just (min,newHeap)

                    return result

        _ ->
            runST $
                do
                    arr <- unsafeThawSTArray minHeap
                    min <- unsafeReadSTArray arr 1
                    largest <- unsafeReadSTArray arr currentSize
                    unsafeWriteSTArray arr 1 largest
                    sink currentSize largest arr
                    newMinHeap <- unsafeFreezeSTArray arr
                    let
                        newHeap =
                            Heap (currentSize - 1) maxSize newMinHeap

                        result =
                            Just (min,newHeap)

                    return result



sink
    :: Ord a
    => Int
    -> a
    -> STArray s Int a
    -> ST s ()

sink currentSize largest arr =
    go 1
    where

        go index =
            do
                let
                    leftIndex =
                        index * 2

                    hasLeft =
                        leftIndex <= currentSize

                    rightIndex =
                        leftIndex + 1

                    hasRight =
                        rightIndex <= currentSize

                    readLeft =
                        unsafeReadSTArray arr leftIndex

                    readRight =
                        unsafeReadSTArray arr rightIndex

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
                        unsafeWriteSTArray arr smallestIndex largest
                        unsafeWriteSTArray arr index smallest
                        go smallestIndex





insert
    :: Ord a
    => a
    -> MinHeap a
    -> MinHeap a

insert new Heap{..} =
    undefined



bubble
    :: Ord a
    => Int
    -> STArray s Int a
    -> ST s ()

bubble index arr =
    undefined
