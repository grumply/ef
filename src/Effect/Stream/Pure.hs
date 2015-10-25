{-# LANGUAGE ExistentialQuantification, DataKinds, MultiParamTypeClasses, FlexibleContexts, BangPatterns #-}
module Main where
-- module Effect.Stream.Pure where

import Effect.Weave

import Mop.Core
import Data.Functor.Identity

import Unsafe.Coerce

import Prelude hiding (map,sum,enumFromTo)
import qualified Prelude

data Step k
  = forall a. Yield a
  | forall a. Next (Maybe a -> k)
data Streaming k = Streaming k

streaming :: Monad m => Attribute Streaming fs m
streaming = Streaming return

instance Pair Streaming Step

newtype Stream a
  = Stream ((a -> Plan '[Step] Identity ()) -> Plan '[Step] Identity ())

{-# INLINE fromList #-}
fromList :: [a] -> Stream a
fromList xs = stream $ \yld -> fromList' yld xs
  where
    fromList' yld = go
      where

        go [] = return ()
        go (x : xs) = yld x >> go xs

{-# INLINE toList #-}
toList :: Stream a -> [a]
toList (Stream x) = go (x (\a -> self (Yield a)))
  where
    go p =
      case p of
        Step sym bp ->
          case prj sym of
            ~(Just a) ->
              case a of
                Yield a -> (unsafeCoerce a):go (bp (unsafeCoerce ()))
        M (Identity p') -> go p'
        Pure () -> []

{-# INLINE stream #-}
stream :: (    (a -> Plan '[Step] Identity ())
            -> Plan '[Step] Identity ()
          ) -> Stream a
stream = Stream

{-# INLINE restream #-}
restream :: Stream a
         -> (    Plan '[Step] Identity (Maybe a)
              -> (b -> Plan '[Step] Identity ())
              -> Plan '[Step] Identity ()
            )
         -> Stream b
restream (Stream s0) x = stream $ \y -> go (s0 (unsafeCoerce y)) (x (self (Next id)) y)
  where
    go s = go'
      where
        go' p =
          case p of
            Step sym bp ->
              case prj sym of
                ~(Just x) ->
                  case x of
                    Next _ -> withNext (unsafeCoerce bp) s
                    _ -> Step sym (\b -> go' (bp b))
            M (Identity p') -> go' p'
            Pure () -> Pure ()
    withNext u = go'
      where
        go' p =
          case p of
            Step sym bp ->
              case prj sym of
                ~(Just x) ->
                  case x of
                    Yield a -> go (bp (unsafeCoerce ())) (u (unsafeCoerce (Just a)))
            M (Identity m) -> go' m
            Pure () -> finish (u (unsafeCoerce Nothing))
    finish p =
      case p of
        Step sym bp ->
          case prj sym of
            ~(Just x) ->
              case x of
                Yield _ -> Step sym (\b -> finish (bp b))
                Next _  -> finish (bp (unsafeCoerce Nothing))
        M (Identity m) -> finish m
        Pure () -> Pure ()

{-# INLINE reduce #-}
reduce :: Stream a
       -> (    Plan '[Step] Identity (Maybe a)
            -> Plan '[Step] Identity r
          ) -> r
reduce (Stream s0) x0 = go (s0 (\a -> self (Yield a))) (x0 (self (Next id)))
  where
    go s = go'
      where
        go' p =
          case p of
            Step sym bp ->
              case prj sym of
                ~(Just x) ->
                  case x of
                    Next _ -> next (unsafeCoerce bp) s
            M (Identity m) -> go' m
            Pure r -> r
    next x' = go'
      where
        go' p =
          case p of
            Step sym bp ->
              case prj sym of
                ~(Just x) ->
                  case x of
                    Yield a -> go (bp (unsafeCoerce ())) (x' (unsafeCoerce (Just a)))
            M (Identity m) -> go' m
            Pure () -> finish (x' (unsafeCoerce Nothing))
    finish p =
      case p of
        Step sym bp ->
          case prj sym of
            ~(Just x) ->
              case x of
                Next _ -> finish (bp (unsafeCoerce Nothing))
        M (Identity m) -> finish m
        Pure r -> r

map :: (a -> b) -> Stream a -> Stream b
map f as = restream as go
  where
    go nxt yld = go'
      where
        go' = do
          ma <- nxt
          case ma of
            Just a -> yld (f a) >> go'
            Nothing -> return ()
{-# INLINE sum #-}
sum :: Num n => Stream n -> n
sum ns = reduce ns go
  where
    go nxt = go' 0
      where
        go' !n = do
          ma <- nxt
          case ma of
            Just a -> go' (n + a)
            Nothing -> return n

strmng :: Object '[Streaming] Identity
strmng = Object $ streaming *:* Empty

wvng :: Object '[Weaving] Identity
wvng = Object $ weaving *:* Empty

{-# INLINE enumFromTo #-}
enumFromTo start end = stream go
  where
    go yld = go' start
      where
        go' !cur = do
          yld cur
          if cur == end
          then return ()
          else go' (cur + 1)

main = do
  --let (_,sm) = runIdentity $ delta strmng (return $ sum $ enumFromTo 1 (10000000 :: Integer))
  -- let (_,sm) = runIdentity $ delta wvng (linearize $ prod (10000000 :: Int) >-> sumr)
  let sm = Prelude.sum [1..10000000 :: Integer]
  print sm

{-# INLINE prod #-}
prod :: Int -> Producer '[Weave] (Maybe Int) Identity a
prod n = producer go
  where
    go yld = go' 0
      where
        go' cur = do
          yld (Just cur)
          let cur' = cur + 1
          if cur == n
          then done
          else go' cur'
        done = yld Nothing >> done

{-# INLINE sumr #-}
sumr :: Consumer '[Weave] (Maybe Int) Identity Int
sumr = consumer go
  where
    go awt = go' 0
      where
        go' !cur = do
          mn <- awt
          case mn of
            Just n -> go' (cur + n)
            Nothing -> return cur
