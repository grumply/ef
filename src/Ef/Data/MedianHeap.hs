{-# LANGUAGE RecordWildCards #-}
module Ef.Data.MedianHeap
    ( Heap
    , empty
    , emptySize

    , singleton
    , singletonSize

    , fromList
    , fromListSize

    , fromAscList
    , fromAscListSize

    , fromDescList
    , fromDescListSize

    , isEmpty
    , isFull

    , insert
    , extractMedian

    , grow
    , shrink
    , fit

    ) where

import qualified Ef.Data.MinHeap as Min
import qualified Ef.Data.MaxHeap as Max

import Control.Monad

import Data.Binary
import Data.IORef
import Data.Maybe

import System.IO.Unsafe



data Heap a =
    Heap
        {
          lessThan
              :: {-# UNPACK #-} !(Max.Heap a)

        , median
              :: {-# UNPACK #-} !(IORef (Maybe a))

        , greaterThan
              :: {-# UNPACK #-} !(Min.Heap a)

        } deriving (Eq)



showHeap
    :: Show a
    => Heap a
    -> String

showHeap Heap {..} =
    "Heap\n\t{" ++
    "\n\tlessThan:\n\t\t" ++ Max.showHeap lessThan ++
    "\n\t,median:\n\t\t" ++ show (unsafePerformIO (readIORef median)) ++
    "\n\t,greaterThan:\n\t\t" ++ Min.showHeap greaterThan ++
    "\n\t}"



instance ( Binary a
         , Ord a
         )
    => Binary (Heap a)
    where

        put Heap {..} =
            do

                put lessThan
                let
                    middle =
                        unsafePerformIO $
                            readIORef median

                put middle
                put greaterThan



        get =
            do
                n <- getWord8
                case n of

                    0 ->
                        let
                            heap =
                                unsafePerformIO $
                                    do
                                        lessThan <- Max.empty
                                        greaterThan <- Min.empty
                                        median <- newIORef Nothing
                                        return Heap {..}

                        in
                            heap `seq` return heap

                    1 ->
                        do
                            lessThan <- get
                            middle <- get
                            let
                                median =
                                    unsafePerformIO $
                                        newIORef middle

                            greaterThan <- get
                            return Heap {..}



-- | O(1) create an empty MedianHeap.
empty
    :: Ord a
    => IO (Heap a)

empty =
    do
        lessThan <- Max.empty
        median <- newIORef Nothing
        greaterThan <- Min.empty
        return Heap {..}



-- | O(1) seed a new MedianHeap with 1 element.
singleton
    :: Ord a
    => a
    -> IO (Heap a)

singleton a =
    do
        lessThan <- Max.empty
        median <- newIORef (Just a)
        greaterThan <- Min.empty
        return Heap {..}



-- | O(1) seed a new MedianHeap of a given size with 1 element.
singletonSize
    :: Ord a
    => Int
    -> a
    -> IO (Heap a)

singletonSize size a =
    do
        heap <- emptySize size
        insert a heap
        return heap



-- | O(1) construct an empty MedianHeap of the given size.
emptySize
    :: Ord a
    => Int
    -> IO (Heap a)

emptySize size
    | size < 0 =
          error "MedianHeap.emptySize: size must be greater than or equal to 0."

    | otherwise =
          do
              let
                  half =
                      size `div` 2

              lessThan <- Max.emptySize half
              median <- newIORef Nothing
              greaterThan <- Min.emptySize half
              return Heap {..}



-- | O(n) constuct a median heap from a given list and sort.
fromList
    :: Ord a
    => [a]
    -> IO (Heap a)

fromList as =
    do
        heap <- initialize
        insertAll heap as
    where
        insertAll heap =
            go
            where

                go [] =
                    return heap

                go (x:xs) =
                    do
                        insert x heap
                        go xs

        initialize =
            do
                median <- newIORef Nothing
                lessThan <- Max.empty
                greaterThan <- Min.empty
                return Heap {..}



-- | O(min(size,n)) construct a MedianHeap from a list and sort until the Heap conforms to the MedianHeap properties.
fromListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (Heap a)


fromListSize size as =
    do
        heap <- initialize
        insertAll heap as
    where

        insertAll heap =
            go
            where

                go [] =
                    return heap

                go (x:xs) =
                    do
                        insert x heap
                        go xs

        initialize =
            do
                let
                    half =
                        size `div` 2

                lessThan <- Max.emptySize half
                median <- newIORef Nothing
                greaterThan <- Min.emptySize half
                return Heap {..}



-- | O(n) construct a MedianHeap from a list pre-sorted in descending order.
fromDescList
    :: Ord a
    => [a]
    -> IO (Heap a)

fromDescList [] =
    empty

fromDescList [one] =
    do
        lessThan <- Max.empty
        median <- newIORef (Just one)
        greaterThan <- Min.empty
        return Heap {..}

fromDescList (more:less:[]) =
    do
        lessThan <- Max.fromListSize 1 [less]
        median <- newIORef (Just more)
        greaterThan <- Min.empty
        return Heap {..}

fromDescList as =
    let
        count =
            length as

    in
        fromDescListSize count as



-- | O(n) construct a MedianHeap from a list pre-sorted in ascending order.
fromAscList
    :: Ord a
    => [a]
    -> IO (Heap a)

fromAscList [] =
    empty

fromAscList [one] =
    do
        lessThan <- Max.empty
        median <- newIORef (Just one)
        greaterThan <- Min.empty
        return Heap {..}

fromAscList (less:more:[]) =
    do
        lessThan <- Max.fromListSize 1 [less]
        median <- newIORef (Just more)
        greaterThan <- Min.empty
        return Heap {..}

fromAscList as =
    do
        let
            count =
                length as

        fromAscListSize count as



-- | O(min(size,n)) construct a Heap from the given list pre-sorted
-- in descending order. If length list > size, the list will be trimmed to fit.
fromDescListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (Heap a)

fromDescListSize size as
    | size < 0 =
          error "MedianHeap.fromDescListSize: size must be greater than or equal to 0."
    | size == 1 =
          case as of

              [] ->
                  emptySize size

              (a:_) ->
                  singletonSize size a

    | otherwise =
          do
              let
                  elements =
                      take size as

                  half =
                      size `div` 2

                  (greater,less) =
                      splitAt half elements

                  middle =
                      head less

                  lesser =
                      tail less

              lessThan <- Max.fromDescListSize (half - 1) lesser
              median <- newIORef (Just middle)
              greaterThan <- Min.fromDescListSize half greater
              rebalance Heap {..}
              return Heap {..}



-- | O(min(size,n)) construct a Heap from the given list pre-sorted
-- in descending order. If length list > size, the list will be trimmed to fit.

fromAscListSize
    :: Ord a
    => Int
    -> [a]
    -> IO (Heap a)

fromAscListSize size as
    | size < 0 =
          error "MedianHeap.fromAscListSize: size must be greater than or equal to 0."

    | size == 1 =

          case as of

              [] ->
                  emptySize size

              (a:_) ->
                  singletonSize size a

    | otherwise =
          do
              let
                  elements =
                      take size as

                  half =
                      size `div` 2

                  (lesser,more) =
                      splitAt half elements

                  middle =
                      head more

                  greater =
                      tail more

              lessThan <- Max.fromAscListSize half lesser
              median <- newIORef (Just middle)
              greaterThan <- Min.fromAscListSize (half - 1) greater
              return Heap {..}



-- | O(log_2 n) insert a value into a MedianHeap.
insert
    :: Ord a
    => a
    -> Heap a
    -> IO ()

insert new Heap {..} =
    do
        mayMiddle <- readIORef median
        case mayMiddle of

            Nothing ->
                writeIORef median (Just new)

            Just middle ->
                case compare new middle of

                    GT ->
                        do
                            Min.insert new greaterThan
                            rebalance Heap {..}

                    _ ->
                        do
                            Max.insert new lessThan
                            rebalance Heap {..}



rebalance Heap {..} =
    do
        mayMiddle <- readIORef median
        case mayMiddle of

            Nothing ->
                return ()

            Just middle ->
                do
                    lessThanSize <- Max.size lessThan
                    greaterThanSize <- Min.size greaterThan
                    if lessThanSize - greaterThanSize > 0 then
                        do
                            max <- Max.extractMax lessThan
                            writeIORef median max
                            Min.insert middle greaterThan
                    else
                        when (greaterThanSize - lessThanSize > 0) $
                            do
                                min <- Min.extractMin greaterThan
                                Just oldMedian <- readIORef median
                                writeIORef median min
                                Max.insert middle lessThan



-- | O(log_2 n) extract the median element.
extractMedian
    :: Ord a
    => Heap a
    -> IO (Maybe a)

extractMedian Heap {..} =
    do
        middle <- readIORef median
        case middle of

            Nothing ->
                return Nothing

            _ ->
                do
                    rebalance
                    return middle

    where
        rebalance =
            do
                lessThanSize <- Max.size lessThan
                greaterThanSize <- Min.size greaterThan
                case compare lessThanSize greaterThanSize of

                    LT ->
                        do
                            newMiddle <- Min.extractMin greaterThan
                            writeIORef median newMiddle

                    _ ->
                        do
                            newMiddle <- Max.extractMax lessThan
                            writeIORef median newMiddle



-- | O(1) view the median value without extracting it.
viewMedian
    :: Heap a
    -> IO (Maybe a)
    
viewMedian Heap {..} =
    readIORef median



-- | O(n) shrink a MedianHeap to the given size.
shrink
    :: Int
    -> Heap a
    -> IO ()

shrink size Heap {..} =
    do
        let
            half =
                size `div` 2

        Max.shrink half lessThan
        Min.shrink half greaterThan



-- | O(n) grow a MedianHeap by one level as if it were a complete binary tree.
grow
    :: Heap a
    -> IO ()

grow Heap {..} =
    do
        Max.grow lessThan
        Min.grow greaterThan



-- | O(n) shrink a MedianHeap to fit its contents.
fit
    :: Heap a
    -> IO ()

fit Heap {..} =
    do
        Max.fit lessThan
        Min.fit greaterThan



-- | O(1) determine if MedianHeap is empty.
isEmpty
    :: Heap a
    -> IO Bool

isEmpty Heap {..} =
    do
        mayMiddle <- readIORef median
        return (isNothing mayMiddle)



-- | O(1) determine if MedianHeap is full.
isFull
    :: Heap a
    -> IO Bool

isFull Heap {..} =
    do
        isHeapEmpty <- isEmpty Heap {..}
        if isHeapEmpty then
            return False
        else
            do
                lessThanFull <- Max.isFull lessThan
                greaterThanFull <- Min.isFull greaterThan
                return (lessThanFull && greaterThanFull)



{-# INLINE empty #-}
{-# INLINE emptySize #-}
{-# INLINE fromList #-}
{-# INLINE fromListSize #-}
{-# INLINE fromAscList #-}
{-# INLINE fromAscListSize #-}
{-# INLINE fromDescList #-}
{-# INLINE fromDescListSize #-}
{-# INLINE insert #-}
{-# INLINE extractMedian #-}
{-# INLINE isEmpty #-}
{-# INLINE isFull #-}
{-# INLINE rebalance #-}
{-# INLINE shrink #-}
{-# INLINE grow #-}
{-# INLINE fit #-}
