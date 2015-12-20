{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified MinHeap as Min
import qualified MaxHeap as Max

import Control.Monad

import Data.Binary
import Data.IORef

import System.IO.Unsafe

import qualified Data.Sequence as Seq

import System.CPUTime
import System.Random

main =
    do
        gen <- newStdGen
        let
            rs =
                take 1000000 (randoms gen)
                
        (rs :: [Int]) `seq` 
                       do
                           test1 rs
                           test2 rs
    where
        test1 rs =
            do
                start <- getCPUTime
                heap <- empty
                fill heap rs
                middle <- extractMedian heap
                end <- middle `seq` getCPUTime
                print (end - start,middle)
            where
               
                fill heap =
                    go
                    where
                    
                        go [] =
                            return ()
                            
                        go (x:xs) =
                            do
                                insert x heap
                                go xs
        test2 rs =
            do
                start <- getCPUTime
                let 
                    sequence = 
                        Seq.fromList (rs :: [Int])

                    middle =
                        flip Seq.index 500000 (Seq.unstableSort sequence)

                            

                middle `seq` 
                    do
                        end <- getCPUTime
                        print (end - start,middle)



main' =
    do
        let heap = Seq.fromList [1..10000000 :: Int]
        case Seq.viewl (Seq.drop 5000000 heap) of

            a Seq.:< _ -> print a

main'' =
    do
        heap <- fromAscListSize 10000000 [1..10000000 :: Int]
        middle <- extractMedian heap
        print middle

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



empty
    :: Ord a
    => IO (Heap a)

empty =
    do
        lessThan <- Max.empty
        median <- newIORef Nothing
        greaterThan <- Min.empty
        return Heap {..}



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



{-# INLINE fromList #-}
{-# INLINE fromAscList #-}
{-# INLINE fromAscListSize #-}
{-# INLINE insert #-}
{-# INLINE extractMedian #-}
{-# INLINE fromListSize #-}
