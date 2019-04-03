{-# LANGUAGE RankNTypes #-}
module Ef.Pipes.Prelude where

import Ef (foldn,lift)
import Ef.Pipes

import Control.Monad (when)
import qualified Prelude
import Prelude hiding (
      all
    , and
    , any
    , concat
    , drop
    , dropWhile
    , elem
    , filter
    , head
    , last
    , length
    , map
    , mapM
    , mapM_
    , maximum
    , minimum
    , notElem
    , null
    , or
    , print
    , product
    , read
    , readLn
    , sequence
    , show
    , seq
    , sum
    , take
    , takeWhile
    , zip
    , zipWith
    )

map :: Monad m => (a -> b) -> Pipe a b m r
map f = for cat (\a -> yield (f a))
{-# INLINABLE [1] map #-}

{-# RULES
    "p >-> map f" forall p f . p >-> map f = for p (\a -> yield (f a))

  ; "map f >-> p" forall p f . map f >-> p = (do
        a <- await
        return (f a) ) >~ p
  #-}

mapM :: Monad m => (a -> m b) -> Pipe a b m r
mapM f = for cat $ \a -> do
    b <- lift (f a)
    yield b
{-# INLINABLE [1] mapM #-}

{-# RULES
    "p >-> mapM f" forall p f . p >-> mapM f = for p (\a -> do
        b <- lift (f a)
        yield b )

  ; "mapM f >-> p" forall p f . mapM f >-> p = (do
        a <- await
        b <- lift (f a)
        return b ) >~ p
  #-}

scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Pipe a b m r
scan step begin done = go begin
  where
    go x = do
        yield (done x)
        a <- await
        let x' = step x a
        go $! x'
{-# INLINABLE scan #-}

drop :: Monad m => Int -> Pipe a a m r
drop = go
  where
    go 0 = cat
    go n =  do
        await
        go (n-1)
{-# INLINABLE drop #-}

dropWhile :: Monad m => (a -> Bool) -> Pipe a a m r
dropWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then go
            else do
                yield a
                cat
{-# INLINABLE dropWhile #-}

take :: Monad m => Int -> Pipe a a m ()
take = go
  where
    go 0 = return () 
    go n = do 
        a <- await
        yield a
        go (n-1)
{-# INLINABLE take #-}

takeWhile :: Monad m => (a -> Bool) -> Pipe a a m ()
takeWhile predicate = go
  where
    go = do
        a <- await
        if (predicate a)
            then do
                yield a
                go
            else return ()
{-# INLINABLE takeWhile #-}

last :: Monad m => Producer a m () -> m (Maybe a)
last p0 = do
    x <- next p0
    case x of
        Left   _      -> return Nothing
        Right (a, p') -> go a p'
  where
    go a p = do
        x <- next p
        case x of
            Left   _       -> return (Just a)
            Right (a', p') -> go a' p'
{-# INLINABLE last #-}

filter :: Monad m => (a -> Bool) -> Pipe a a m r
filter predicate = for cat $ \a -> when (predicate a) (yield a)
{-# INLINABLE [1] filter #-}

{-# RULES
    "p >-> filter predicate" forall p predicate.
        p >-> filter predicate = for p (\a -> when (predicate a) (yield a))
  #-}

concat :: (Monad m, Foldable f) => Pipe (f a) a m r
concat = for cat each
{-# INLINABLE [1] concat #-}

{-# RULES
    "p >-> concat" forall p . p >-> concat = for p each
  #-}

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Producer a m () -> m b
fold step begin done p0 = 
  foldn (\_ x -> return (done x)) (\m x -> m >>= ($ x)) go p0 begin 
  where
    go p x = case p of
        Request v  _  -> closed v
        Respond a  fu -> fu () $! step x a
{-# INLINABLE fold #-}

toListM :: Monad m => Producer a m () -> m [a]
toListM = fold step begin done
  where
    step x a = x . (a:)
    begin = id
    done x = x []
{-# INLINABLE toListM #-}

zip :: Monad m
    => (Producer   a     m r)
    -> (Producer      b  m r)
    -> (Producer' (a, b) m r)
zip = zipWith (,)
{-# INLINABLE zip #-}

zipWith :: Monad m
    => (a -> b -> c)
    -> (Producer  a m r)
    -> (Producer  b m r)
    -> (Producer' c m r)
zipWith f = go
  where
    go p1 p2 = do
        e1 <- lift $ next p1
        case e1 of
            Left r         -> return r
            Right (a, p1') -> do
                e2 <- lift $ next p2
                case e2 of
                    Left r         -> return r
                    Right (b, p2') -> do
                        yield (f a b)
                        go p1' p2'
{-# INLINABLE zipWith #-}

unfoldr :: Monad m 
        => (s -> m (Either r (a, s))) -> s -> Producer a m r
unfoldr step = go where
  go s0 = do
    e <- lift (step s0)
    case e of
      Left r -> return r
      Right (a,s) -> do 
        yield a
        go s
{-# INLINABLE unfoldr #-}
