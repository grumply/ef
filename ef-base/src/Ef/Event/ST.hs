{-# language BangPatterns #-}
module Ef.Event.ST where

import Ef
import Ef.Narrative

import Data.Queue
import Data.Promise

import Control.Monad
import Data.List
import System.Mem.Weak
import Unsafe.Coerce
import Data.STRef.Strict
import Control.Monad.ST

import qualified Data.Sequence as Seq

data Event k where
  Become :: (event -> Narrative '[Event] (ST s) ())
         -> k
         -> Event k

  Continue :: Event k

  End :: Event k

  Subsignal :: Signal' s event'
            -> event'
            -> k
            -> Event k

become :: (event -> Narrative '[Event] (ST s) ())
       -> Narrative '[Event] (ST s) ()
become f = self $ Become f ()

continue :: Narrative '[Event] (ST s) a
continue = self Continue

end :: Narrative '[Event] (ST s) a
end = self End

subsignal :: Signal' s event
          -> event
          -> Narrative '[Event] (ST s) ()
subsignal sig e = self $ Subsignal sig e ()

data Signal' s event
    = Signal
        (STRef s ([(STRef s (event -> Narrative '[Event] (ST s) ()))]))
    deriving Eq

data Behavior' s event
  = Behavior
      (STRef s (event -> Narrative '[Event] (ST s) ()))
  deriving Eq

construct :: ST s (Signal' s event)
construct = do
  behaviors <- newSTRef []
  return $ Signal behaviors

-- Slightly more specific than necessary to avoid required type signatures.
runner :: ST s (Signal' s (ST s ()))
runner = do
  bhvr <- newSTRef super
  behaviors <- newSTRef [bhvr]
  return $ Signal behaviors

behavior :: Signal' s event
         -> (event -> Narrative '[Event] (ST s) ())
         -> ST s (Behavior' s event)
behavior sig@(Signal behaviors) newBehavior = do
  b <- newSTRef newBehavior
  modifySTRef behaviors (++ [b])
  return (Behavior b)

mergeS :: Signal' s event
       -> Signal' s event
       -> ST s ( Signal' s event
               , Behavior' s event
               , Behavior' s event
               )
mergeS sig0 sig1 = do
  sig <- construct
  bt0 <- behavior sig0 $ super . signal sig
  bt1 <- behavior sig1 $ super . signal sig
  return (sig,bt0,bt1)

zipS :: Signal' s event
     -> Signal' s event'
     -> ST s ( Signal' s (Maybe event,Maybe event')
             , Behavior' s event
             , Behavior' s event'
             )
zipS = zipWithS (,)

-- holds onto old value references even if a signal falls out of scope.
zipWithS :: (Maybe event -> Maybe event' -> x)
         -> Signal' s event
         -> Signal' s event'
         -> ST s ( Signal' s x
                 , Behavior' s event
                 , Behavior' s event'
                 )
zipWithS f sig0@(Signal bs0) sig1@(Signal bs1) = do
  sig <- construct
  c0 <- newSTRef Nothing
  c1 <- newSTRef Nothing
  bt0 <- behavior sig0 $ \e0 -> super $ do
    mc1 <- readSTRef c1
    signal sig $ f (Just e0) mc1
  bt1 <- behavior sig1 $ \e1 -> super $ do
    mc0 <- readSTRef c0
    signal sig $ f mc0 (Just e1)
  return (sig,bt0,bt1)

mapS :: Signal' s event
     -> (event -> event')
     -> ST s ( Signal' s event'
             , Behavior' s event
             )
mapS sig f = do
  sig' <- construct
  bt   <- behavior sig $ super . signal sig' . f
  return (sig',bt)

map2S :: Signal' s event0
      -> Signal' s event1
      -> (Either event0 event1 -> event2)
      -> ST s ( Signal' s event2
              , Behavior' s event0
              , Behavior' s event1
              )
map2S sig0 sig1 f = do
  sig <- construct
  bt0 <- behavior sig0 $ super . signal sig . f . Left
  bt1 <- behavior sig1 $ super . signal sig . f . Right
  return (sig,bt0,bt1)

filterS :: Signal' s event
        -> (event -> Maybe event')
        -> ST s ( Signal' s event'
                , Behavior' s event
                )
filterS sig f = do
  sig' <- construct
  bt   <- behavior sig $ \e -> super $ forM_ (f e) (signal sig')
  return (sig',bt)

filter2S :: Signal' s event0
         -> Signal' s event1
         -> (Either event0 event1 -> Maybe event)
         -> ST s ( Signal' s event
                 , Behavior' s event0
                 , Behavior' s event1
                 )
filter2S sig0 sig1 f = do
  sig <- construct
  bt0 <- behavior sig0 $ \e -> super $ forM_ (f $ Left e) (signal sig)
  bt1 <- behavior sig1 $ \e -> super $ forM_ (f $ Right e) (signal sig)
  return (sig,bt0,bt1)

duplicate :: Behavior' s event
          -> Signal' s event
          -> ST s ()
duplicate (Behavior b) (Signal bs_) =
  modifySTRef bs_ (++ [b])

-- stop is a delayed effect that doesn't happen until the next event, but all
-- internally maintained references are released; stopping an un-needed behavior
-- can permit GC external to the behavior itself.
stop :: Behavior' s a -> ST s ()
stop (Behavior b_) =
    modifySTRef b_ $ const (const end)

data Runnable s where
  Runnable :: STRef s ([(STRef s
                          (event -> Narrative '[Event] (ST s) ()
                          )
                        )
                       ]
                      )
           -> STRef s (event -> Narrative '[Event] (ST s) ())
           -> Narrative '[Event] (ST s) ()
           -> Runnable s
{-# INLINE signal #-}
signal :: forall s event. Signal' s event -> event -> ST s ()
signal sig e = do
  let Signal bs_ = sig
  bs <- readSTRef bs_
  seeded <- forM bs $ \f_ -> do
    f <- readSTRef f_
    return $ Runnable bs_ f_ (f e)
  go seeded
  where
    {-# INLINE go #-}
    go :: [Runnable s] -> ST s ()
    go [] = return ()
    go (r@(Runnable bs_ f_ f):rs) = start rs r
      where
        {-# INLINE start #-}
        start :: [Runnable s] -> Runnable s -> ST s ()
        start rs (Runnable bs_ f_ f) = go' f
          where
            {-# INLINE go' #-}
            go' :: Narrative '[Event] (ST s) ()
                -> ST s ()
            go' (Return _)  = go rs

            go' (Fail e)    = return () -- throw away exceptions

            go' (Super sup) = sup >>= go'
            go' (Say msg k) =
              case prj msg of
                ~(Just x) ->
                  case x of
                    Become f' x -> do
                      writeSTRef f_ $ unsafeCoerce f'
                      go' (k x)
                    Continue -> return ()
                    End      -> modifySTRef bs_ (Prelude.filter (/= f_))
                    Subsignal sig' e' x -> do
                      let Signal bs'_ = sig'
                      bs' :: [(STRef s (event -> Narrative '[Event] (ST s) ()))]
                        <- readSTRef (unsafeCoerce bs'_)
                      seeded <- forM bs' $ \f'_ -> do
                        f' <- readSTRef f'_
                        return (Runnable bs'_ (unsafeCoerce f'_) ((unsafeCoerce f') e'))
                      go (((Runnable bs_ f_ (k x)) : rs) ++ unsafeCoerce seeded)
