module Effect.Reactive where

import Mop.Core
import Mop.IO
import Effect.Interleave hiding (FreshScope)
import Effect.Exception hiding (FreshScope)

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

data Behavior a fs m = Behavior
  { cultured :: Int
  , behavior :: IORef (a -> Plan fs m ())
  }

data Culture a fs m = Culture
  { cultureId :: Int
  , culture :: IORef (a -> Plan fs m ())
  , behaviors :: IORef [Behavior a fs m]
  }

data React k
  = FreshScope (Int -> k)
  | forall a fs m. NewCulture Int (Culture a fs m -> k)
  | forall a fs m. NewBehavior Int (Culture a fs m) (a -> Plan fs m ()) (Behavior a fs m -> k)
  | forall a fs m. ModifyBehavior Int (Behavior a fs m) (a -> Plan fs m ()) k
  | forall a fs m. TriggerBehavior Int (Behavior a fs m) a k
  | forall a fs m. TriggerCulture Int (Culture a fs m) a k
  | forall a fs m. DestroyBehavior Int (Behavior a fs m) k
  | forall a fs m. DestroyCulture Int (Culture a fs m) k

data Reactive k = Reactive Int k

{-# INLINE reactive #-}
reactive :: Uses Reactive fs m => Attribute Reactive fs m
reactive = Reactive 0 nextS
  where
    nextS fs =
      let Reactive s ns = (fs&)
          s' = succ s
      in s' `seq` pure (fs .= Reactive s' ns)

instance Pair Reactive React where
  pair p (Reactive i k) (FreshScope ik) = p k (ik i)

data ReactorSystem fs m = Reactor
  { newC :: forall a. Plan fs m (Culture a fs m)
  , newB :: forall a. Culture a fs m -> (a -> Plan fs m ()) -> Plan fs m (Behavior a fs m)
  , modB :: forall a. Behavior a fs m -> (a -> Plan fs m ()) -> Plan fs m ()
  , triggerB :: forall a. Behavior a fs m -> a -> Plan fs m ()
  , triggerC :: forall a. Culture a fs m -> a -> Plan fs m ()
  , destroyB :: forall a. Behavior a fs m -> Plan fs m ()
  , destroyC :: forall a. Culture a fs m -> Plan fs m ()
  }

{-# INLINE react #-}
react :: forall fs m a. (Has React fs m,Has Throw fs m,MIO m,Has Interleave fs m)
      => (    ReactorSystem fs m
           -> Plan fs m a
         ) -> Plan fs m a
react f = do
  scope <- self (FreshScope id)
  sys <- unsafe $ newIORef []
  transform sys scope 0 $ f Reactor
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
        go :: forall b. Int -> Plan fs m b -> Plan fs m b
        go c = go'
          where
            go' :: forall b. Plan fs m b -> Plan fs m b
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
                           TriggerCulture i c a _ -> check i $ do
                             triggerCulture c a
                             go' (bp (unsafeCoerce ()))
                           DestroyBehavior i b _ -> check i $ do
                             unsafe $ destroyBehavior (unsafeCoerce b)
                             go' (bp (unsafeCoerce ()))
                           DestroyCulture i c _ -> check i $ do
                             unsafe $ destroyCulture (unsafeCoerce c)
                             go' (bp (unsafeCoerce ()))
                           _ -> ignore
                       _ -> ignore
                M m -> M (fmap go' m)
                Pure r -> Pure r

            newCulture cultureId = unsafe $ do
              culture <- newIORef (const (return ()) :: forall a. a -> Plan fs m ())
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
                    compile' [] = return () :: Plan fs m ()
                    compile' (b:bs) = do
                      b a
                      compile' bs

            modifyBehavior b f = unsafe $ do
              let c = cultured b
              writeIORef (behavior b) f
              cus <- lookupCulture c
              case cus of
                (cu:_) -> recompile cu
                _ -> return ()
              where
                lookupCulture c = do
                  cs <- readIORef sys
                  return $ filter (\Culture{..} -> cultureId == c) cs
                recompile Culture{..} = do
                  bhs <- mapM (readIORef . behavior) =<< readIORef behaviors
                  writeIORef culture $ \a -> go' (compile a bhs)
                  where
                    compile a = compile'
                      where
                        compile' [] = return () :: Plan fs m ()
                        compile' (b:bs) = do
                          b a
                          compile' bs

            triggerBehavior b a = do
              let bh = unsafePerformIO $ readIORef (behavior b)
              bh `seq` go' (unsafeCoerce bh a)

            triggerCulture c a = do
              let cu = unsafePerformIO $ readIORef (culture c)
              cu `seq` go' (unsafeCoerce cu a)

            destroyBehavior b = do
              writeIORef (behavior b) (const (return ()) :: forall a. a -> Plan fs m ())
            destroyCulture c = do
              writeIORef (culture c) (const (return ()) :: forall a. a -> Plan fs m ())
              bs <- readIORef (behaviors c)
              mapM_ destroyBehavior bs
