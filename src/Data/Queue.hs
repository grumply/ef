module Data.Queue where

-- amortized constant-time queue
data Queue a = Queue [a] [a]

{-# INLINE emptyQueue #-}
emptyQueue = Queue [] []

{-# INLINE newQueue #-}
newQueue stack = Queue stack []

{-# INLINE enqueue #-}
enqueue :: a -> Queue a -> Queue a
enqueue a (Queue l r) = Queue l (a:r)

{-# INLINE dequeue #-}
dequeue :: Queue a -> Maybe (Queue a,a)
dequeue (Queue [] []) = Nothing
dequeue (Queue [] xs) =
    let stack = reverse xs
    in Just (Queue (tail stack) [],head stack)
dequeue (Queue xs ys) = Just (Queue (tail xs) ys,head xs)