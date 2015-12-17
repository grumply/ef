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
        heap =
            new [1..5000000 :: Int]

    print (fmap fst (extractMin heap))
    print (fmap fst (extractMin ))



data MinHeap a =
    Heap
        {
          stats
              :: {-# UNPACK #-} !(Array Int Int)

        , minHeap
              :: {-# UNPACK #-} !(Array Int a)

        }



instance Show a => Show (MinHeap a)
    where

        show (Heap stats minHeap) =
            "Heap"
                ++ "  {"
                       ++ "\n\t\tcurrentSize: " ++ show (unsafeAt stats 0)
                       ++ "\n\t\tmaxSize: " ++ show (unsafeAt stats 1)
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
            
        stats =
            unsafeArray' (0,1) 2 [(0,currentSize),(1,maxSize)]

        minHeap =
            listArray (0,maxSize) []

    in
        Heap{..}



-- | Create a new min-heap from the given size and list, unsafely.
unsafeNew
    :: Ord a
    => [a]
    -> MinHeap a

unsafeNew as =
    let
        currentSize =
            length as

        maxSize =
            heightToSize (sizeToHeight currentSize)
        
        stats =
            unsafeArray' (0,1) 2 [(0,currentSize),(1,maxSize)]

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
    (unsafeAt stats 0) == 0



unsafeSort
    :: Ord a
    => MinHeap a
    -> MinHeap a

unsafeSort Heap{..} = Heap {..}
    -- runST $
    --     do
    --         undefined



unsafeViewMin
    :: Ord a
    => MinHeap a
    -> Maybe a

unsafeViewMin Heap{..} =
    if (unsafeAt stats 0) == 0 then
        Nothing
    else
        Just (unsafeAt minHeap 1)



unsafeExtractMin
    :: forall a.
       (Ord a,Show a)
    => MinHeap a
    -> Maybe (a,MinHeap a)

unsafeExtractMin Heap{..} =
    --trace (show $ Heap {..}) $
    case unsafeAt stats 0 of

        0 ->
            Nothing

        1 ->
            runST $
                do
                    arr <- unsafeThawSTArray minHeap
                    min <- unsafeReadSTArray arr 1
                    newMinHeap <- unsafeFreezeSTArray arr
                    let
                        result =
                            Just (min,newHeap)

                    return result

        _ ->
            runST $
                do
                    let
                        !currentSize =
                            unsafeAt stats 0
                            
                        !maxSize =
                            unsafeAt stats 1
                            
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
    where

        sink
            :: Int
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


{-# INLINE heightToSize #-}
{-# INLINE sizeToHeight #-}
{-# INLINE unsafeExtractMin #-}
{-# INLINE insert #-}
{-# INLINE bubble #-}
{-# INLINE unsafeNew #-}
{-# INLINE unsafeViewMin #-}
{-# INLINE empty #-}
{-# INLINE sizedEmpty #-}
