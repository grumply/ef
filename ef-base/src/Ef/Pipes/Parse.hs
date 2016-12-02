{-# language ImpredicativeTypes #-}
{-# language ConstraintKinds #-}
module Ef.Pipes.Parse where

import Ef
import Ef.Pipes
import Ef.State

import Control.Monad
import Data.Functor.Constant
import Data.Maybe

import Prelude hiding (span)

import Unsafe.Coerce

data Producing a m = forall x. Producing { fromProducing :: Producer a m x }

type Parser a m r = forall x. Narrative '[State (Producing a m)] m r

getProducer :: forall a m x. Monad m => Parser a m (Producer a m x)
getProducer = project <$> get
  where
    project :: Producing a m -> Producer a m x
    project (Producing p) = unsafeCoerce p

putProducer :: Monad m => Producer a m x -> Parser a m ()
putProducer = put . Producing

draw :: forall m a. (Monad m, MonadIO m, MonadThrow m) => Parser a m (Maybe a)
draw = do
  p <- getProducer
  x <- lift (next p)
  case x of
    Left r -> do
      putProducer $ return r
      return (Nothing :: Maybe a)
    Right (a,p') -> do
      putProducer p'
      return (Just a)

skip :: (Monad m, MonadIO m, MonadThrow m) => Parser a m Bool
skip = do
  x <- draw
  return $ isJust x

drawAll :: (Monad m, MonadIO m, MonadThrow m) => Parser a m [a]
drawAll = go id
  where
    go diffAs = do
      x <- draw
      case x of
        Nothing -> return (diffAs [])
        Just a -> go (diffAs . (a:))

skipAll :: (Monad m, MonadIO m, MonadThrow m) => Parser a m ()
skipAll = go
  where
    go = do
      x <- draw
      forM_ x (const go)

unDraw :: Monad m => a -> Parser a m ()
unDraw a = do
  p <- getProducer
  putProducer (producer $ \yield -> yield a >> runProxy p)

peek :: (Monad m, MonadIO m, MonadThrow m) => Parser a m (Maybe a)
peek = do
  x <- draw
  forM_ x unDraw
  return x

isEndOfInput :: (Monad m, MonadIO m, MonadThrow m) => Parser a m Bool
isEndOfInput = do
  x <- peek
  return $ isNothing x

foldAll :: (Monad m, MonadIO m, MonadThrow m) => (x -> a -> x) -> x -> (x -> b) -> Parser a m b
foldAll step begin done = go begin
  where
    go x = do
      ea <- draw
      case ea of
        Nothing -> return (done x)
        Just a -> go $! step x a

foldAllM :: (Monad m, MonadIO m, MonadThrow m) => (x -> a -> m x) -> m x -> (x -> m b) -> Parser a m b
foldAllM step begin done = do
  x0 <- lift begin
  go x0
  where
    go x = do
      ea <- draw
      case ea of
        Nothing -> lift (done x)
        Just a -> do
          x' <- lift (step x a)
          go $! x'

type Lens' a b = forall f. (Functor f) => (b -> f b) -> a -> f a

span :: (Monad m, MonadThrow m) => (a -> Bool) -> Lens' (Producer a m x) (Producer a m (Producer a m x))
span predicate k p0 = fmap join (k (producer to))
  where
    to yield = go p0
      where
        go p = do
          x <- lift (next p)
          case x of
            Left r -> return (return r)
            Right (a,p') ->
              if predicate a
              then yield a >> go p'
              else return $ (Proxy $ yield a) >> p'

splitAt :: (Monad m, MonadThrow m) => Int -> Lens' (Producer a m x) (Producer a m (Producer a m x))
splitAt n0 k p0 = fmap join (k (producer to))
  where
    to yield = go n0 p0
      where
        go n p =
          if n <= 0
          then return p
          else do
            x <- lift (next p)
            case x of
              Left r -> return (return r)
              Right (a, p') -> do
                yield a
                go (n - 1) p'

(^.) :: a -> ((b -> Constant b b) -> a -> Constant b a) -> b
a ^. lens = getConstant (lens Constant a)

groupBy :: (Monad m, MonadThrow m) => (a -> a -> Bool) -> Lens' (Producer a m x) (Producer a m (Producer a m x))
groupBy equals k p0 = fmap join (k (to p0))
  where
    to p = do
      x <- lift (next p)
      case x of
        Left r -> return (return r)
        Right (a,p') ->
          ((producer $ \yield -> yield a) >> p') ^. span (equals a)

group :: (Monad m, MonadThrow m, Eq a) =>  Lens' (Producer a m x) (Producer a m (Producer a m x))
group = groupBy (==)

toParser :: (Monad m, MonadIO m, MonadThrow m) => Consumer (Maybe a) m r -> Parser a m r
toParser consumer = runEffect (lift draw >~ Proxy (unsafeHoist lift $ runProxy consumer))

toParser_ :: (Monad m, MonadThrow m) => Consumer a m X -> Parser a m ()
toParser_ consumer = do
  p <- getProducer
  r <- lift $ runEffect $ p >-> fmap closed consumer
  return r

parsed :: (Monad m, MonadThrow m)
       => Parser a m (Either e b)
       -> Producer a m r -> Producer b m (e, Producer a m r)
parsed parser = go
  where
    go p = do
      (_,(x,p')) <- lift $ Object (state (Producing p) *:* Empty) $. do
        x <- parser
        p' <- getProducer
        return (x,p')
      case x of
        Left r -> return (r,p')
        Right b -> do
          producer $ \yield -> yield b
          go p'

parsed_ :: (Monad m, MonadThrow m)
        => Parser a m (Maybe b)
        -> Producer a m r
        -> Producer b m (Producer a m r)
parsed_ parser = go
  where
    go p = do
      ((),p') <- parsed parser' p
      return p'
      where
        parser' = do
          x <- parser
          return $
            case x of
              Nothing -> Left ()
              Just b -> Right b
