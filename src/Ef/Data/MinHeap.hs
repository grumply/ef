{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Data.MinHeap
    ( Heap

    , empty
    , emptySize

    , fromList
    , fromListSize
    , toList

    , rawFromList
    , rawFromListSize
    , rawToList

    , fromAscList
    , fromAscListSize

    , fromDescList
    , fromDescListSize

    , extractMin
    , viewMin

    , insert

    , isEmpty
    , isFull
    , size
    , canHold

    , shrink
    , fit
    , grow

    , heightToSize
    , sizeToHeight

    , showHeap
    ) where

import Control.Monad
import Data.List (intersperse)
import qualified Data.List as List
import Data.Array
import Data.Array.IO
import Data.Array.Base
import Data.Array.Unsafe
import Data.IORef
import System.IO.Unsafe

import Data.Binary

main =
    do
        queue <- rawFromList [1,2 :: Int]
        min <- extractMin queue
        next <- extractMin queue
        print (min,next)

data Heap a =
    Heap
        {
          currentSize
              :: {-# UNPACK #-} !(IORef Int)

        , maxSize
              :: {-# UNPACK #-} !(IORef Int)

        , minHeap
              :: {-# UNPACK #-} !(IORef (IOArray Int a))

        } deriving (Eq)



instance forall a.
          Binary a
    => Binary (Heap a)
    where

        get =
            do
                arr <- get
                let
                    (_,curSize) =
                        Data.Array.bounds arr

                    make =
                        unsafePerformIO $
                            do
                                currentSize <- newIORef curSize
                                maxSize     <- newIORef curSize
                                heap        <- unsafeThaw arr
                                minHeap     <- newIORef heap
                                return Heap {..}

                make `seq` return make


        put Heap {..} =
            let
                arr =
                    unsafePerformIO $
                        do
                            heap <- readIORef minHeap
                            unsafeFreeze heap

            in
                arr `seq` put (arr :: Array Int a)



-- | O(n) convert a heap to a human-readable representation (for debugging). Use
-- the Binary implementation for storage and transmission.
showHeap
    :: Show a
    => Heap a
    -> String

showHeap Heap{..} =
    "Heap"
        ++ "  {"
               ++ "\n\t\tcurrentSize: "
                   ++ show (unsafePerformIO $ readIORef currentSize)
               ++ "\n\t\tmaxSize: "
                   ++ show (unsafePerformIO $ readIORef maxSize)
               ++ "\n\t\tminHeap: ["
                   ++ concat (intersperse ","
                               (showHeap (unsafePerformIO $ readIORef minHeap))
                             )
                   ++ "]\n"
        ++ "  }"
    where

        showHeap heap =
            go 1
            where

                go index =
                    let
                        curSize =
                            unsafePerformIO (readIORef currentSize)

                        valueAtIndex =
                            unsafePerformIO (readArray heap index)

                    in
                        if index > curSize then
                            []
                        else
                            show valueAtIndex : go (succ index)



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
    => IO (Heap a)

empty =
    emptySize (heightToSize 2)



-- | Create an empty Heap of the given size.
emptySize
    :: Ord a
    => Int
    -> IO (Heap a)

emptySize size =
    do
        heap        <- newArray_ (0,size)
        currentSize <- newIORef 0
        maxSize     <- newIORef size
        minHeap     <- newIORef heap
        return Heap {..}



-- | O(n log_2 n) convert a Heap to an ordered list. The returned list is
-- spine strict.
toList
    :: (Ord a,Show a)
    => Heap a
    -> IO [a]

toList heap =
    do
        list <- go
        list `seq` return list
    where

        go =
            do
                mayValue <- extractMin heap
                case mayValue of

                    Nothing ->
                        return []

                    Just value ->
                        do
                            rest <- go
                            return (value:rest)



-- | O(n) convert a Heap to an unorderd list with a guarantee that
-- the smallest element is at the head. The returned list is spine-strict.
rawToList
    :: Heap a
    -> IO [a]

rawToList Heap{..} =
    do
        heap    <- readIORef minHeap
        curSize <- readIORef currentSize
        list    <- withHeap heap curSize
        list `seq` return list
    where

        withHeap heap =
            go
            where

                go 0 =
                    return []

                go n =
                    do
                        value <- unsafeRead heap n
                        rest  <- go (pred n)
                        return (value:rest)



-- | O(n) create a new min-heap from the given list. If you know the length of the
-- list ahead of time, use 'fromListSize'.
fromList
    :: Ord a
    => [a]
    -> IO (Heap a)

fromList as =
    do
        let
            count =
                length as

            size =
                heightToSize (sizeToHeight count)

        heap        <- newListArray (0,size) (undefined:as)
        currentSize <- newIORef count
        maxSize     <- newIORef size
        minHeap     <- newIORef heap
        sort   Heap {..}
        return Heap {..}



-- | O(n) construct a MinHeap from the given list pre-sorted in MinHeap order.
rawFromList
    :: Ord a
    => [a]
    -> IO (Heap a)

rawFromList as =
    do
        let
            count =
               length as

            size =
               heightToSize (sizeToHeight count)

        heap        <- newListArray (0,size) (undefined:as)
        currentSize <- newIORef count
        maxSize     <- newIORef size
        minHeap     <- newIORef heap
        return Heap {..}



-- | O(n) construct a MinHeap from the given size and the given list pre-sorted
-- in MinHeap order. If the length of the list is < size, the list will be
-- trimmed to fit.
rawFromListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (Heap a)

rawFromListSize =
    fromAscListSize



-- | O(min(size,n)) construct a Heap from the given list, as, and
-- size by taking size elements from the list. If size < length list, this
-- function will trim the list to size.
fromListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (Heap a)

fromListSize size as =
    do
        let
            elements =
                take size as

        heap        <- newListArray (0,size) (undefined:elements)
        currentSize <- newIORef size
        maxSize     <- newIORef size
        minHeap     <- newIORef heap
        sort   Heap {..}
        return Heap {..}



-- | O(n) construct a Heap from the given list pre-sorted in ascending order.
fromAscList
    :: Ord a
    => [a]
    -> IO (Heap a)

fromAscList =
    rawFromList



-- | O(min(size,n)) construct a Heap of a given size with the given
-- list pre-sorted in ascending order. If length list > size, the list will
-- be trimmed to fit.
fromAscListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (Heap a)

fromAscListSize size as =
    let
        elements =
            take size as

    in
        do
            heap        <- newListArray (0,size) (undefined:elements)
            currentSize <- newIORef size
            maxSize     <- newIORef size
            minHeap     <- newIORef heap
            return Heap {..}



-- | O(n) construct a Heap from the given list pre-sorted in descending
-- order. The Heap will be fitted to the length of the list.
fromDescList
    :: Ord a
    => [a]
    -> IO (Heap a)

fromDescList as =
    let
        count =
            length as

        size =
            heightToSize (sizeToHeight count)

    in
        do
            heap        <- newArray_ (0,size)
            currentSize <- newIORef count
            maxSize     <- newIORef size
            minHeap     <- newIORef heap
            fill heap count as
            return Heap {..}
    where

        {-# INLINE fill #-}
        fill heap =
            go
            where

                {-# INLINE go #-}
                go !_ [] =
                    return ()

                go n (x:xs) =
                    do
                        unsafeWrite heap n x
                        go (pred n) xs



-- | O(min(size,n)) construct a Heap from the given list pre-sorted
-- in descending order. If length list > size, the list will be trimmed to fit.
fromDescListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (Heap a)

fromDescListSize size as =
    let
        count =
            length as

    in
        do
            heap        <- newArray_ (0,count)
            currentSize <- newIORef count
            maxSize     <- newIORef count
            minHeap     <- newIORef heap
            fill heap count as
            return Heap {..}
    where

        {-# INLINE fill #-}
        fill heap =
            go
            where

                {-# INLINE go #-}
                go !_ [] =
                    return ()

                go n (x:xs) =
                    do
                        unsafeWrite heap n x
                        go (pred n) xs



-- | O(1) view the current size of the Heap.
size
    :: Heap a
    -> IO Int

size Heap {..} =
    readIORef currentSize



-- | O(1) view the maximum size of the Heap.
canHold
    :: Heap a
    -> IO Int

canHold Heap {..} =
    readIORef maxSize



-- | O(1) check if a Heap is empty.
isEmpty
    :: Heap a
    -> IO Bool

isEmpty Heap{..} =
    let
        getCurrentSize =
            readIORef currentSize

        isZero =
            (== 0)

    in
        fmap isZero getCurrentSize



-- | O(1) check if the Heap is full.
isFull
    :: Heap a
    -> IO Bool

isFull Heap{..} =
    do
        currentSize <- readIORef currentSize
        maxSize     <- readIORef maxSize
        return (currentSize == maxSize)



-- | O(n) grow a heap as if it were a complete tree.
grow
    :: Heap a
    -> IO ()

grow Heap{..} =
    do
        max <- readIORef maxSize
        let
            growMaxSize =
                heightToSize . succ . sizeToHeight

            newMaxSize =
                growMaxSize max

        curSize <- readIORef currentSize
        oldHeap <- readIORef minHeap
        newHeap <- newArray_ (0,newMaxSize)

        copy oldHeap newHeap curSize

        writeIORef minHeap newHeap
        writeIORef maxSize newMaxSize
    where

        {-# INLINE copy #-}
        copy old new =
            go
            where

                {-# INLINE go #-}
                go 0 =
                    return ()

                go n =
                    do
                        value <- unsafeRead old n
                        unsafeWrite new n value
                        go (pred n)



-- | O(n) shrink a Heap to the given size. Drops arbitrary elements
-- if Heap is larger than size.
shrink
    :: Int
    -> Heap a
    -> IO ()

shrink size Heap{..} =
    do
        curSize <- readIORef currentSize
        oldHeap <- readIORef minHeap
        newHeap <- newArray_ (0,size)
        let
            updateMaxSize =
                writeIORef maxSize size

            replaceArray =
                writeIORef minHeap newHeap

            copyElements =
                copy oldHeap newHeap curSize

        replaceArray
        copyElements
        updateMaxSize
    where

        {-# INLINE copy #-}
        copy old new =
            copyElement
            where

                {-# INLINE copyElement #-}
                copyElement 0 =
                    return ()

                copyElement n =
                    do
                        value <- unsafeRead old n
                        unsafeWrite new n value
                        copyElement (pred n)



-- | O(n) shrink a Heap to fit its contents.
fit
    :: Heap a
    -> IO ()

fit Heap {..} =
    do
        curSize <- readIORef currentSize
        shrink curSize Heap{..}



-- | O(n) sort a Heap to conform to the Heap property
-- that every child is greater than or equal to its parent.
sort
    :: Ord a
    => Heap a
    -> IO ()

sort Heap {..} =
    do
        curSize <- readIORef currentSize
        heap    <- readIORef minHeap
        let
            middle =
                curSize `div` 2

            upperHalfOfHeap =
                [middle,pred middle .. 1]

        forM_ upperHalfOfHeap (sink curSize heap)
    where
        
        sink curSize heap =
            sinkElement
            where

                sinkElement !index =
                    do
                        sinking <- unsafeRead heap index
                        let
                            leftIndex =
                                index * 2

                            hasLeft =
                                leftIndex <= curSize

                            rightIndex =
                                succ leftIndex

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
                                sinkElement smallestIndex



-- | O(1) view the minimum value in a Heap.
viewMin
    :: Heap a
    -> IO (Maybe a)

viewMin Heap{..} =
    do
        currentSize <- readIORef currentSize
        if currentSize == 0 then
            return Nothing
        else
            do
                heap <- readIORef minHeap
                min  <- unsafeRead heap 1
                return (Just min)



-- | O(log_2 n) extract the minimum value from a Heap.
extractMin
    :: (Show a,Ord a)
    => Heap a
    -> IO (Maybe a)

extractMin Heap{..} =
    do
        heap    <- readIORef minHeap
        curSize <- readIORef currentSize
        print (showHeap Heap {..})
        if curSize == 0 then
            return Nothing
        else
            do
                min     <- unsafeRead heap 1
                largest <- unsafeRead heap curSize
                unsafeWrite heap 1 largest
                sink curSize heap 1

                let
                    !newCurSize =
                         pred curSize

                writeIORef currentSize newCurSize
                print (showHeap Heap {..})
                return (Just min)
    where

        sink curSize heap =
            sinkElement
            where

                sinkElement !index =
                    do
                        sinking <- unsafeRead heap index
                        let
                            leftIndex =
                                index * 2

                            hasLeft =
                                leftIndex <= curSize

                            rightIndex =
                                succ leftIndex

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
                                sinkElement smallestIndex



-- | O(log_2 n) insert a value into a Heap.
insert
    :: Ord a
    => a
    -> Heap a
    -> IO ()
    
insert new Heap{..} =
    do
        curSize <- growIfNecessary
        addElement curSize
    where

        addElement curSize =
            do
                heap <- readIORef minHeap
                let
                    newCurrentSize =
                        succ curSize

                    updateCurrentSize =
                        writeIORef currentSize newCurrentSize

                    insertNew =
                        unsafeWrite heap newCurrentSize new

                insertNew
                updateCurrentSize
                bubble heap newCurrentSize

        growIfNecessary =
            do
                let
                    canHoldNew =
                        do
                            maxSize <- readIORef maxSize
                            curSize <- readIORef currentSize
                            return (maxSize > curSize,curSize)

                (canHold,curSize) <- canHoldNew
                unless canHold (grow Heap {..})
                return curSize

        bubble arr =
            go
            where
                    
                go index =
                    let
                        parent =
                            index `div` 2

                    in
                        when (parent > 0) $
                            do
                                parentValue <- unsafeRead arr parent
                                when (parentValue > new) $
                                    do
                                        unsafeWrite arr parent new
                                        unsafeWrite arr index parentValue
                                        go parent



{-# INLINE empty #-}
{-# INLINE emptySize #-}

{-# INLINE fromList #-}
{-# INLINE fromListSize #-}

{-# INLINE fromAscList #-}
{-# INLINE fromAscListSize #-}

{-# INLINE fromDescList #-}
{-# INLINE fromDescListSize #-}

{-# INLINE extractMin #-}
{-# INLINE viewMin #-}

{-# INLINE insert #-}

{-# INLINE isEmpty #-}
{-# INLINE isFull #-}

{-# INLINE toList #-}
{-# INLINE rawToList #-}

{-# INLINE shrink #-}
{-# INLINE fit #-}
{-# INLINE grow #-}

{-# INLINE sort #-}

{-# INLINE heightToSize #-}
{-# INLINE sizeToHeight #-}
