module Effect.Reactive where

import Mop.Core
import Mop.IO
import Effect.Interleave hiding (FreshScope)
import Effect.Exception hiding (FreshScope)

import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

-- | Neuron represents the identity of a specific unknown set of axons that can
-- be used for triggering all axons contained therein.
newtype Neuron a = Neuron Int

-- | Axon represents the location of a specific behavior as the identity of a
-- specific Neuron combined with the identity of the behavior itself.
newtype Axon a = Axon (Int,Int)

-- | React is the DSL for Reactive programming. These language primitives are
-- implemented in 'ReactiveScope' and used in 'react' scopes.
data React k
  = FreshScope (Int -> k)

  | forall a.            NewN Int (Neuron a -> k)
  | forall a.        TriggerN Int (Neuron a) a k
  | forall fs m a.   DestroyN Int (Neuron a) k

  | forall fs m a b.     NewA Int (Neuron a) (a -> Plan fs m ()) (Axon a -> k)
  | forall a.        TriggerA Int (Axon a) a k
  | forall fs m a.   DestroyA Int (Axon a) k

-- | Reactive is the attribute that enables 'React' capabilities.
data Reactive k = Reactive Int k k
reactive :: Uses Reactive fs m => Attribute Reactive fs m
reactive = flip (Reactive 0) return $ \fs ->
  let Reactive i k k' = (fs&)
      i' = succ i
  in i' `seq` pure (fs .= Reactive i' k k')

instance Pair Reactive React where
  pair p (Reactive i k _) (FreshScope ik)     = p k (ik i)
  pair p (Reactive _ _ k) (TriggerN _ _ _ k') = p k k'
  pair p (Reactive _ _ k) (TriggerA _ _ _ k') = p k k'
  pair p (Reactive _ _ k) (DestroyA _ _ k')   = p k k'
  pair p (Reactive _ _ k) (NewA _ _ _ bk)     = p k (bk undefined)
  pair p (Reactive _ _ k) (NewN _ ek)         = p k (ek undefined)

-- | ReactScope represents the capabilities for a reactive component. Use of
-- these capabilities is enabled inside a 'react' scope.
data ReactScope fs m = React
  { newN     :: !(forall a. Plan fs m (Neuron a))
  , triggerN :: !(forall a. Neuron a -> a -> Plan fs m ())
  , destroyN :: !(forall a. Neuron a -> Plan fs m ())
  , newA     :: !(forall a. Neuron a -> (a -> Plan fs m ()) -> Plan fs m (Axon a))
  , triggerA :: !(forall a. Axon a -> a -> Plan fs m ())
  , destroyA :: !(forall a. Axon a -> Plan fs m ())
  }

data Brain fs m = Brain
  { neuronCount :: Int
  , axonCount :: Int
  , neuronalConfiguration :: !(forall a. [(Neuron a,[(Axon a,a -> Plan fs m ())])])
  }

{-# INLINE react #-}
react :: forall fs m a. (Has React fs m,Has Interleave fs m,MIO m,Has Throw fs m)
         => (    ReactScope fs m
              -> Plan fs m a
            )
         -> Plan fs m a
react f = do
  scope <- self (FreshScope id)
  sys_ <- unsafe (newIORef (Brain 0 0 []))
  interleave $ \frk _ ->
    transform scope frk sys_ $ f React
      { newN = self (NewN scope id)
      , triggerN = \ev a -> self (TriggerN scope ev a ())
      , destroyN = \ev -> self (DestroyN scope ev ())
      , newA = \ev f -> self (NewA scope ev f id)
      , triggerA = \b a -> self (TriggerA scope b a ())
      , destroyA = \b -> self (DestroyA scope b ())
      }
  where
    {-# INLINE transform #-}
    transform scope frk sys_ = tangible
      where
        {-# INLINE tangible #-}
        tangible :: forall b. Plan fs m b -> Plan fs m b
        tangible p =
          case p of
            Step sym bp ->
              let check i x = if i == scope then x else ignore
                  ignore = Step sym (\b -> tangible (bp b))
              in case prj sym of
                   Just x ->
                     case x of

                       NewN i _ -> check i $ do
                         sys <- unsafe (readIORef sys_)
                         let eS = neuronCount sys
                             eS' = eS + 1
                         unsafe $ writeIORef sys_ $ sys { neuronCount = eS' }
                         eS' `seq`
                           Step sym $ const $ tangible $
                             bp (unsafeCoerce (Neuron eS))

                       TriggerN i ev a _ -> check i $
                         Step sym $ const $ tangible $ do
                           triggerNeuron ev a
                           bp (unsafeCoerce ())

                       TriggerA i b a _ -> check i $
                         Step sym $ const $ tangible $ do
                           triggerAxon b a
                           bp (unsafeCoerce ())

                       DestroyN i ev _ -> check i $ do
                         destroyNeuron ev
                         Step sym $ const $ tangible $
                           bp (unsafeCoerce ())

                       NewA i ev@(Neuron e) f _ -> check i $ do
                         sys <- unsafe $ readIORef sys_
                         let bS = axonCount sys
                             bS' = bS + 1
                             b = Axon (e,bS)
                         unsafe $ writeIORef sys_ $ sys { axonCount = bS' }
                         newAxon b f
                         bS' `seq`
                           Step sym $ const $ tangible $
                             bp (unsafeCoerce b)

                       DestroyA i b _ -> check i $ do
                         destroyAxon b
                         Step sym $ const $ tangible $
                           bp (unsafeCoerce ())

                       _ -> ignore
                   Nothing -> ignore
            M m -> M (fmap tangible m)
            Pure r -> Pure r

        {-# INLINE intangible #-}
        intangible :: forall b. Plan fs m b -> Plan fs m b
        intangible p =
          case p of
            Step sym bp ->
              let check i x = if i == scope then x else ignore
                  ignore = Step sym (\b -> intangible (bp b))
              in case prj sym of
                   Just x ->
                     case x of
                       NewN i _ -> check i $ do
                         sys <- unsafe $ readIORef sys_
                         let eS = neuronCount sys
                             eS' = eS + 1
                         unsafe $ writeIORef sys_ $ sys { neuronCount = eS' }
                         eS' `seq` intangible $ bp (unsafeCoerce (Neuron eS))

                       TriggerN i ev a _ -> check i $ intangible $ do
                         triggerNeuron ev a
                         bp (unsafeCoerce ())

                       TriggerA i b a _ -> check i $ intangible $ do
                         triggerAxon b a
                         bp (unsafeCoerce ())

                       DestroyN i ev _ -> check i $ do
                         destroyNeuron ev
                         intangible $ bp (unsafeCoerce ())

                       NewA i ev@(Neuron e) f _ -> check i $ do
                         sys <- unsafe $ readIORef sys_
                         let bS = axonCount sys
                             bS' = bS + 1
                             b = Axon (e,bS)
                         unsafe $ writeIORef sys_ $ sys { axonCount = bS' }
                         newAxon b f
                         bS' `seq` intangible $ bp (unsafeCoerce b)

                       DestroyA i b _ -> check i $ do
                         destroyAxon b
                         intangible $ bp (unsafeCoerce ())

                       _ -> ignore
                   Nothing -> ignore
            M m -> M (fmap intangible m)
            Pure r -> Pure r

        {-# INLINE triggerNeuron #-}
        triggerNeuron (Neuron e) a = do
            sys <- unsafe $ readIORef sys_
            go $ neuronalConfiguration sys
          where
            go [] = return ()
            go (eb@(Neuron i,bs):ebs)
              | i == e = go' bs
              | otherwise = go ebs
              where
                go' [] = return ()
                go' ((_,f):bs) = frk (intangible $ f (unsafeCoerce a)) >> go' bs

        {-# INLINE triggerAxon #-}
        triggerAxon (Axon (n,i)) a = do
            sys <- unsafe $ readIORef sys_
            go $ neuronalConfiguration sys
          where
            go [] = return ()
            go (eb@(Neuron i,bs):ebs)
              | i == n = go' bs
              | otherwise = go ebs
              where
                go' [] = return ()
                go' ((Axon (_,a'),f):bs)
                  | i == a' = frk (intangible $ f (unsafeCoerce a))
                  | otherwise = go' bs

        {-# INLINE destroyNeuron #-}
        destroyNeuron (Neuron e) = unsafe $ modifyIORef sys_ $ \sys ->
            sys { neuronalConfiguration = go (neuronalConfiguration sys) }
          where
            go [] = []
            go (eb@(Neuron i,_):ebs)
              | i == e = ebs
              | otherwise = eb:go ebs

        {-# INLINE newAxon #-}
        newAxon b@(Axon (e,bh)) f = unsafe $ modifyIORef sys_ $ \sys ->
            sys { neuronalConfiguration = go (neuronalConfiguration sys) }
          where
            go [] = [(unsafeCoerce (Neuron bh),[(unsafeCoerce b,unsafeCoerce f)])]
            go (eb@(Neuron i,bs):ebs)
              -- performance leak to guarantee FIFO
              | i == e = (Neuron i,bs ++ [(unsafeCoerce b,unsafeCoerce f)]):ebs
              | otherwise = eb:go ebs

        {-# INLINE destroyAxon #-}
        destroyAxon (Axon (e,b)) = unsafe $ modifyIORef sys_ $ \sys ->
            sys { neuronalConfiguration = go' (neuronalConfiguration sys) }
          where
            go' [] = []
            go' (eb@(Neuron i,bs):ebs)
              | i == e = (Neuron i,go'' bs):ebs
              | otherwise = eb:go' ebs

            go'' [] = []
            go'' (bh@(Axon (_,b'),_):bs)
              | b == b' = bs
              | otherwise = bh:go'' bs
