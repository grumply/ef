{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Data.List (intersperse,unfoldr)
import qualified Data.List as List
import Data.Array.IO
import Data.Array.Base
import Data.Array.Unsafe
import Data.IORef
import System.IO.Unsafe



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
        heap <- fromAscListSize 10000000 [1..10000000 :: Int]
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
    emptySize (heightToSize 2)



-- | Create an empty MinHeap of the given size.
emptySize
    :: Ord a
    => Int
    -> IO (MinHeap a)

emptySize size =
    do
        heap <- newArray_ (0,size)
        currentSize <- newIORef 0
        maxSize <- newIORef size
        minHeap <- newIORef heap

        return Heap{..}


-- | O(n log_2 n) convert a MinHeap to an ordered list. The returned list is
-- spine strict.
toList
    :: Ord a
    => MinHeap a
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


-- | O(n) convert a MinHeap to an unorderd list with a guarantee that
-- the smallest element is at the head. The returned list is spine-strict.
rawToList
    :: MinHeap a
    -> IO [a]

rawToList Heap{..} =
    do
        heap <- readIORef minHeap
        curSize <- readIORef currentSize
        list <- withHeap heap curSize
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
                        rest <- go (n - 1)
                        return (value:rest)



-- | O(n) create a new min-heap from the given list. If you know the length of the
-- list ahead of time, use 'fromListSize'.
fromList
    :: Ord a
    => [a]
    -> IO (MinHeap a)

fromList as =
    do
        let
            count =
                length as

            size =
                heightToSize (sizeToHeight count)

        heap <- newListArray (0,size) (undefined:as)
        currentSize <- newIORef count
        maxSize <- newIORef size
        minHeap <- newIORef heap
        sort Heap{..}
        return Heap {..}


-- | O(min(size,length as)) construct a MinHeap from the given list, as, and 
-- size by taking size elements from the list. If size < length list, this 
-- function will trim the list to size.
fromListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (MinHeap a)
    
fromListSize size as =
    do
        let
            elements =
                take size as
                
        heap <- newListArray (0,size) (undefined:elements)
        currentSize <- newIORef size
        maxSize <- newIORef size
        minHeap <- newIORef heap
        sort Heap {..}
        return Heap {..}


-- | O(n) construct a MinHeap from the given list pre-sorted in ascending order.
-- The MinHeap will be fitted to the size of the list. 
fromAscList
    :: Ord a
    => [a]
    -> IO (MinHeap a)

fromAscList as =
    let
        count =
            length as

    in
        do
            heap <- newListArray (0,count) (undefined:as)
            currentSize <- newIORef count
            maxSize <- newIORef count
            minHeap <- newIORef heap
            return Heap{..}


-- | O(min(size,length)) construct a MinHeap of a given size with the given
-- list pre-sorted in ascending order. If length list > size, the list will 
-- be trimmed to fit.
fromAscListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (MinHeap a)
    
fromAscListSize size as =
    let
        elements =
            take size as
            
    in
        do
            heap <- newListArray (0,size) (undefined:elements)
            currentSize <- newIORef size
            maxSize <- newIORef size
            minHeap <- newIORef heap
            return Heap {..}


-- | O(n) construct a MinHeap from the given list pre-sorted in descending 
-- order. The MinHeap will be fitted to the length of the list.
fromDescList
    :: Ord a
    => [a]
    -> IO (MinHeap a)

fromDescList as =
    let
        count =
            length as

        size =
            heightToSize (sizeToHeight count)

    in
        do
            heap <- newArray_ (0,size)
            fill heap count as
            currentSize <- newIORef count
            maxSize <- newIORef size
            minHeap <- newIORef heap
            return Heap{..}
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
                        go (n - 1) xs



-- | O(min(size,length as)) construct a MinHeap from the given list pre-sorted
-- in descending order. If length list > size, the list will be trimmed to fit.
fromDescListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (MinHeap a)

fromDescListSize size as =
    let
        count =
            length as

    in
        do
            heap <- newArray_ (0,count)
            fill heap count as
            currentSize <- newIORef count
            maxSize <- newIORef count
            minHeap <- newIORef heap
            return Heap{..}
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
                        go (n - 1) xs



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
    -> IO ()

grow Heap{..} =
    do
        max <- readIORef maxSize
        curSize <- readIORef currentSize
        let
            newMaxSize =
                heightToSize . succ . sizeToHeight $ max

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
                        go (n - 1)


shrink
    :: Int
    -> MinHeap a
    -> IO ()
    
shrink size Heap{..} =
    do
        curSize <- readIORef currentSize
        oldHeap <- readIORef minHeap
        newHeap <- newArray_ (0,size)
        copy oldHeap newHeap curSize
        writeIORef minHeap newHeap
        writeIORef maxSize size
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
                        go (n - 1)


fit
    :: MinHeap a
    -> IO ()

fit Heap {..} =
    do
        curSize <- readIORef currentSize
        shrink curSize Heap{..}



sort
    :: Ord a
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
    :: Ord a
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

        {-# INLINE withCurrentSize #-}
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
        when (curSize == max) (grow Heap{..})
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
        
        {-# INLINE go #-}
        go index =
            do
                let
                    parent =
                        index `div` 2

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
{-# INLINE insert #-}

{-# INLINE viewMin #-}

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

{-# INLINE sink #-}
{-# INLINE bubble #-}
