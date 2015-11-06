{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PostfixOperators  #-}
module Effect.Interleave where

import Mop.Core
import Data.Queue

import Unsafe.Coerce

data Interleave k
  = forall fs m a. Fork Int (Plan fs m a)
  | forall fs m a. Atomically Int (Plan fs m a)
  | Stop Int
  | FreshScope (Int -> k)

data Interleaving k = Interleaving Int k
{-# INLINE interleaves #-}
interleaves :: Uses Interleaving gs m => Attribute Interleaving gs m
interleaves = Interleaving 0 $ \fs ->
  let Interleaving i k = (fs&)
      i' = succ i
  in i' `seq` pure $ fs .= Interleaving i' k

instance Pair Interleaving Interleave where
  pair p (Interleaving i k) (FreshScope ik) = p k (ik i)

{-# INLINE interleave #-}
-- interleave $ \fork atomically ->
interleave :: forall fs m a. Has Interleave fs m
           => (    (Plan fs m () -> Plan fs m ())
                -> (forall b. Plan fs m b -> Plan fs m b)
                -> Plan fs m a
              ) -> Plan fs m a
interleave f = do
  scope <- self (FreshScope id)
  scoped scope $
      (f (\p -> self (Fork scope (p >> self (Stop scope))) >> Pure ())
         (\p -> self (Atomically scope p))
      )
  where
    scoped scope = start
      where
        start p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Fork i child ->
                      if i == scope
                      then rooted (enqueue (unsafeCoerce child) emptyQueue) (bp (unsafeCoerce ()))
                      else Step sym $ \b -> start (bp b)
                    Atomically i atom ->
                      if i == scope
                      then start $ unsafeCoerce atom >>= \b -> start (bp (unsafeCoerce b))
                      else Step sym $ \b -> start (bp b)
                    Stop i ->
                      if i == scope
                      then Pure (unsafeCoerce ())
                      else Step sym $ \b -> start (bp b)
                    _ -> Step sym (\b -> start (bp b))
                Nothing -> Step sym (\b -> start (bp b))
            M m -> M (m >>= \p' -> return $ start p')
            Pure r -> Pure r

        rooted rest p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Fork i child ->
                      if i == scope
                      then rooted (enqueue (unsafeCoerce child) rest) (bp (unsafeCoerce ()))
                      else Step sym $ \b -> rooted rest (bp b)
                    Atomically i atom ->
                      if i == scope
                      then start (unsafeCoerce atom) >>= \b ->
                             case dequeue rest of
                               Nothing -> start (bp (unsafeCoerce b))
                               Just (rest',nxt) -> rooted (enqueue (bp (unsafeCoerce b)) rest') nxt
                      else Step sym $ \b -> rooted rest (bp b)
                    Stop i ->
                      if i == scope
                      then case dequeue rest of
                             Just (rest',nxt) -> rooted rest' nxt
                             Nothing -> Pure (unsafeCoerce ())
                      else Step sym $ \b -> rooted rest (bp b)
                    _ -> Step sym (\b -> rooted rest (bp b))
                Nothing -> Step sym $ \b ->
                  case dequeue rest of
                    Nothing -> start (bp b)
                    Just (rest',nxt) -> Step sym $ \b -> rooted (enqueue (bp b) rest') nxt
            M m -> M (m >>= \p' ->
                        case dequeue rest of
                          Nothing -> return $ start p'
                          Just (rest',nxt) ->
                            let q = enqueue (unsafeCoerce p') rest'
                            in q `seq` return $ rooted q nxt
                     )
            Pure r ->
              case dequeue rest of
                Nothing -> Pure r
                Just (rest,nxt) -> rooted rest nxt >> Pure r
