{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Lang.Scoped.Alternate
  ( Alternating, Alternate(..), alternates
  , Alternatable, alternator
  ) where

import Ef.Core
import Data.Queue

import Unsafe.Coerce

-- | Symbol

data Alternating k
  = forall fs m a. Fork Int (Pattern fs m a)
  | forall fs m a. Atomically Int (Pattern fs m a)
  | Stop Int
  | FreshScope (Int -> k)

-- | Symbol Module

data Alternate fs m = Alternate
  { alt :: Pattern fs m () -> Pattern fs m ()
  , atomically :: forall b. Pattern fs m b -> Pattern fs m b
  }

-- | Attribute

data Alternatable k = Alternatable Int k

-- | Attribute Construct

{-# INLINE alternator #-}
alternator :: Uses Alternatable gs m => Attribute Alternatable gs m
alternator = Alternatable 0 $ \fs ->
  let Alternatable i k = view fs
      i' = succ i
  in i' `seq` pure $ fs .= Alternatable i' k

-- | Symbol/Attribute Symmetry

instance Symmetry Alternatable Alternating where
  symmetry use (Alternatable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct + Substitution

{-# INLINE alternates #-}
alternates :: forall fs m a. Is Alternating fs m
           => (    Alternate fs m
                -> Pattern fs m a
              ) -> Pattern fs m a
alternates f = do
  scope <- self (FreshScope id)
  substitute scope $ f Alternate
    { alt = \p -> self (Fork scope (p >> self (Stop scope)))
    , atomically = \p -> self (Atomically scope p)
    }
  where
    substitute scope = start
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
                    Just (rest',nxt) -> Step sym $ \b' -> rooted (enqueue (bp b') rest') nxt
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
                Just (rest',nxt) -> rooted rest' nxt >> Pure r
