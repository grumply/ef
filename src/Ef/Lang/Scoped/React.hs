{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Ef.Lang.Scoped.React
  ( Behavior, Culture
  , Reacting, reacts
  , Reactable, reactor
  , React(..)
  ) where

import Ef.Core
import Ef.Lang.IO

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

-- | Symbol

data Behavior a fs m = Behavior
  { cultured :: Int
  , behavior :: IORef (a -> Pattern fs m ())
  }

data Culture a fs m = Culture
  { cultureId :: Int
  , culture :: IORef (a -> Pattern fs m ())
  , behaviors :: IORef [Behavior a fs m]
  }

data Reacting k
  = FreshScope (Int -> k)
  | forall a fs m. NewCulture Int (Culture a fs m -> k)
  | forall a fs m. NewBehavior Int (Culture a fs m) (a -> Pattern fs m ()) (Behavior a fs m -> k)
  | forall a fs m. ModifyBehavior Int (Behavior a fs m) (a -> Pattern fs m ()) k
  | forall a fs m. TriggerBehavior Int (Behavior a fs m) a k
  | forall a fs m. TriggerCulture Int (Culture a fs m) a k
  | forall a fs m. DestroyBehavior Int (Behavior a fs m) k
  | forall a fs m. DestroyCulture Int (Culture a fs m) k

-- | Symbol Module

data React fs m = React
  { newC :: forall a. Pattern fs m (Culture a fs m)
  , newB :: forall a. Culture a fs m -> (a -> Pattern fs m ()) -> Pattern fs m (Behavior a fs m)
  , modB :: forall a. Behavior a fs m -> (a -> Pattern fs m ()) -> Pattern fs m ()
  , triggerB :: forall a. Behavior a fs m -> a -> Pattern fs m ()
  , triggerC :: forall a. Culture a fs m -> a -> Pattern fs m ()
  , destroyB :: forall a. Behavior a fs m -> Pattern fs m ()
  , destroyC :: forall a. Culture a fs m -> Pattern fs m ()
  }

-- | Attribute

data Reactable k = Reactable Int k

-- | Attribute Construct

{-# INLINE reactor #-}
reactor :: Uses Reactable fs m => Attribute Reactable fs m
reactor = Reactable 0 nextS
  where
    nextS fs =
      let Reactable s ns = view fs
          s' = succ s
      in s' `seq` pure (fs .= Reactable s' ns)

-- | Attribute/Symbol Symmetry

instance Witnessing Reactable Reacting where
  witness use (Reactable i k) (FreshScope ik) = use k (ik i)

-- | Local Scoping Construct + Substitution

{-# INLINE reacts #-}
reacts :: forall fs m a. (Is Reacting fs m,Lift IO m)
      => (    React fs m
           -> Pattern fs m a
         ) -> Pattern fs m a
reacts r = do
  scope <- self (FreshScope id)
  sys <- unsafe $ newIORef []
  transform sys scope 0 $ r React
      { newC = self (NewCulture scope id)
      , newB = \c f -> self (NewBehavior scope c f id)
      , modB = \b f -> self (ModifyBehavior scope b f ())
      , triggerB = \b a -> self (TriggerBehavior scope b a ())
      , triggerC = \c a -> self (TriggerCulture scope c a ())
      , destroyB = \b -> self (DestroyBehavior scope b ())
      , destroyC = \c -> self (DestroyCulture scope c ())
      }
  where
    transform sys scope = go
      where
        go :: forall b. Int -> Pattern fs m b -> Pattern fs m b
        go c = go'
          where
            go' :: forall c. Pattern fs m c -> Pattern fs m c
            go' p =
              case p of
                Step sym bp ->
                  let check i x = if i == scope then x else ignore
                      ignore = Step sym (\b -> go' (bp b))
                  in case prj sym of
                       Just x ->
                         case x of
                           NewCulture i _ -> check i $ do
                             cu <- newCulture c
                             let c' = c + 1
                             c' `seq` go c' (bp (unsafeCoerce cu))
                           NewBehavior i cu f _ -> check i $ do
                             b <- newBehavior (unsafeCoerce cu) f
                             go' (bp (unsafeCoerce b))
                           ModifyBehavior i b f _ -> check i $ do
                             modifyBehavior b f
                             go' (bp (unsafeCoerce ()))
                           TriggerBehavior i b a _ -> check i $ do
                             triggerBehavior b a
                             go' (bp (unsafeCoerce ()))
                           TriggerCulture i cu a _ -> check i $ do
                             triggerCulture cu a
                             go' (bp (unsafeCoerce ()))
                           DestroyBehavior i b _ -> check i $ do
                             unsafe $ destroyBehavior (unsafeCoerce b)
                             go' (bp (unsafeCoerce ()))
                           DestroyCulture i cu _ -> check i $ do
                             unsafe $ destroyCulture (unsafeCoerce cu)
                             go' (bp (unsafeCoerce ()))
                           _ -> ignore
                       _ -> ignore
                M m -> M (fmap go' m)
                Pure res -> Pure res

            newCulture cultureId = unsafe $ do
              culture <- newIORef (const (return ()) :: forall d. d -> Pattern fs m ())
              behaviors <- newIORef []
              let cu = Culture{..}
              modifyIORef sys (cu:)
              return cu

            newBehavior cu f = unsafe $ do
              let cultured = cultureId cu
              b0_ <- newIORef f
              let b0 = Behavior cultured b0_
              modifyIORef (behaviors cu) $ \bs ->
                let bs' = (unsafeCoerce b0):bs
                in bs' `seq` bs'
              bhs <- mapM (readIORef . behavior) =<< readIORef (behaviors cu)
              writeIORef (culture cu) $ \a -> go' (compile a bhs)
              return b0
              where
                compile a = compile'
                  where
                    compile' [] = return () :: Pattern fs m ()
                    compile' (b:bs) = do
                      b a
                      compile' bs

            modifyBehavior b f = unsafe $ do
              let cu = cultured b
              writeIORef (behavior b) f
              cus <- lookupCulture cu
              case cus of
                (cid:_) -> recompile cid
                _ -> return ()
              where
                lookupCulture cid = do
                  cs <- readIORef sys
                  return $ filter (\Culture{..} -> cultureId == cid) cs
                recompile Culture{..} = do
                  bhs <- mapM (readIORef . behavior) =<< readIORef behaviors
                  writeIORef culture $ \a -> go' (compile a bhs)
                  where
                    compile a = compile'
                      where
                        compile' [] = return () :: Pattern fs m ()
                        compile' (bh:bhs) = do
                          bh a
                          compile' bhs

            triggerBehavior b a = do
              let bh = unsafePerformIO $ readIORef (behavior b)
              bh `seq` go' (unsafeCoerce bh a)

            triggerCulture cu a = do
              let cul = unsafePerformIO $ readIORef (culture cu)
              cul `seq` go' (unsafeCoerce cul a)

            destroyBehavior b = do
              writeIORef (behavior b) (const (return ()) :: forall d. d -> Pattern fs m ())
            destroyCulture cu = do
              writeIORef (culture cu) (const (return ()) :: forall d. d -> Pattern fs m ())
              bs <- readIORef (behaviors cu)
              mapM_ destroyBehavior bs
