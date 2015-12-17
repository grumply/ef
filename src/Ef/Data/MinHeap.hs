{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Monad.ST
import Data.List
import Debug.Trace
import GHC.Arr


main = do
    let
        minsorted = 
            [undefined,1,2,3,2,3,6,7,2,8,4]

        arr =
            listArray (0,11) minsorted

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
    
    arr `seq` print "Done" -- print (go 10 minHeap)

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
                    go 0
                    where
                        
                        go index
                            | index == currentSize = 
                                  []
                            | otherwise =
                                  let
                                      valueAtIndex =
                                          unsafeAt minHeap index
                                          
                                  in
                                      show valueAtIndex : go (index + 1)
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
                    arr <- thawSTArray minHeap
                    min <- readSTArray arr 1
                    newMinHeap <- freezeSTArray arr
                    let
                        newHeap =
                            Heap 0 maxSize newMinHeap

                        result =
                            Just (min,newHeap)

                    return result

        _ ->
            runST $
                do
                    arr <- thawSTArray minHeap
                    min <- readSTArray arr 1
                    largest <- readSTArray arr currentSize
                    writeSTArray arr 1 largest
                    sink largest arr
                    newMinHeap <- freezeSTArray arr
                    let
                        newHeap =
                            Heap (currentSize - 1) maxSize newMinHeap

                        result =
                            Just (min,newHeap)

                    return result

    where
        sink
            :: a
            -> STArray s Int a
            -> ST s ()
 
        sink largest arr =
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
                                readSTArray arr leftIndex

                            readRight =
                                readSTArray arr rightIndex

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
                                writeSTArray arr smallestIndex largest
                                writeSTArray arr index smallest
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
    -> MinHeap a
    -> MinHeap a

bubble index Heap{..} =
    undefined



sink
    :: Ord a
    => MinHeap a
    -> MinHeap a

sink Heap{..} =
    undefined
