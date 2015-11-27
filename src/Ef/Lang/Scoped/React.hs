{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.React
  ( Behavior
  , Culture
  , Reacting
  , reacts
  , Reactable
  , reactor
  , React(..)
  ) where



import Ef.Core
import Ef.Lang.IO

import Data.Binary
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce



data Behavior a fs m =
    Behavior
        {
          cultured
              :: Int

        , behavior
              :: IORef (a -> Pattern fs m ())
        }



data Culture a fs m =
    Culture
        {
          cultureId
              :: Int

        , culture
              :: IORef (a -> Pattern fs m ())

        , behaviors
              :: IORef [Behavior a fs m]
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



data React fs m =
    React
        {
          newC
              :: forall a.
                 Pattern fs m (Culture a fs m)

        , newB
              :: forall a.
                 Culture a fs m
              -> (    a
                   -> Pattern fs m ()
                 )
              -> Pattern fs m (Behavior a fs m)

        , modB
              :: forall a.
                 Behavior a fs m
              -> (    a
                   -> Pattern fs m ()
                 )
              -> Pattern fs m ()

        , triggerB
              :: forall a.
                 Behavior a fs m
              -> a
              -> Pattern fs m ()

        , triggerC
              :: forall a.
                 Culture a fs m
              -> a
              -> Pattern fs m ()

        , destroyB
              :: forall a.
                 Behavior a fs m
              -> Pattern fs m ()

        , destroyC
              :: forall a.
                 Culture a fs m
              -> Pattern fs m ()
        }



data Reactable k =
    Reactable Int k



instance Uses Reactable gs m
    => Binary (Attribute Reactable gs m)
  where

    get =
        return reactor



    put _ =
        pure ()




reactor
    :: Uses Reactable fs m
    => Attribute Reactable fs m

reactor =
    Reactable 0 nextS
  where
    nextS fs =
        let
          Reactable s ns =
              view fs

          s' =
              succ s

        in
          s' `seq` pure $ fs .=
              Reactable s' ns



instance Reactable `Witnessing` Reacting
  where

    witness use (Reactable i k) (FreshScope ik) =
        use k (ik i)



reacts :: forall fs m a. (Is Reacting fs m,Lift IO m)
      => (    React fs m
           -> Pattern fs m a
         ) -> Pattern fs m a
reacts r = do
    scope <- self (FreshScope id)
    sys <- unsafe $ newIORef []
    rewrite sys scope 0 $ r
        React
            {
              newC =
                  self (NewCulture scope id)

            , newB =
                  \c f ->
                      self (NewBehavior scope c f id)

            , modB =
                  \b f ->
                      self (ModifyBehavior scope b f ())

            , triggerB =
                  \b a ->
                      self (TriggerBehavior scope b a ())

            , triggerC =
                  \c a ->
                      self (TriggerCulture scope c a ())

            , destroyB =
                  \b ->
                      self (DestroyBehavior scope b ())

            , destroyC =
                  \c ->
                      self (DestroyCulture scope c ())
            }
  where

    rewrite sys rewriteScope = go
      where

        go :: forall b. Int -> Pattern fs m b -> Pattern fs m b
        go c = go'
          where

            go' :: forall c. Pattern fs m c -> Pattern fs m c
            go' (Fail err) =
                Fail err

            go' (Pure res) =
                Pure res

            go' (M m) =
                M (fmap go' m)

            go' (Step sym bp) =
                let
                  check currentScope scoped =
                      if currentScope == rewriteScope then
                          scoped
                      else
                          ignore

                  ignore =
                      Step sym (go' . bp)

                in
                  case prj sym of

                      Just x ->
                          case x of

                              NewCulture currentScope _ ->
                                  check currentScope $
                                      do
                                        cu <- newCulture c
                                        let
                                          c' =
                                              c + 1

                                          continue =
                                              bp (unsafeCoerce cu)

                                        c' `seq` go c' continue

                              NewBehavior currentScope cu f _ ->
                                  check currentScope $
                                      do
                                        b <- newBehavior (unsafeCoerce cu) f
                                        let
                                          continue =
                                              bp (unsafeCoerce b)

                                        go' continue

                              ModifyBehavior currentScope b f _ ->
                                  check currentScope $
                                      do
                                        modifyBehavior b f
                                        let
                                          continue =
                                              bp (unsafeCoerce ())

                                        go' continue

                              TriggerBehavior i b a _ ->
                                  check i $
                                      do
                                        _ <- triggerBehavior b a
                                        let
                                          continue =
                                              bp (unsafeCoerce ())

                                        go' continue

                              TriggerCulture i cu a _ ->
                                  check i $
                                      do
                                        _ <- triggerCulture cu a
                                        let
                                          continue =
                                              bp (unsafeCoerce ())

                                        go' continue

                              DestroyBehavior i b _ ->
                                  check i $
                                      do
                                        unsafe $ destroyBehavior (unsafeCoerce b)
                                        let
                                          continue =
                                              bp (unsafeCoerce ())

                                        go' continue

                              DestroyCulture i cu _ -> check i $ do
                                unsafe $ destroyCulture (unsafeCoerce cu)
                                let
                                  continue =
                                      bp (unsafeCoerce ())

                                go' continue

                              _ ->
                                  ignore
                      _ ->
                          ignore

            newCulture cultureId =
                unsafe $
                    do
                      let
                        seed =
                          const (Pure ())

                      culture <- newIORef seed
                      behaviors <- newIORef []
                      let
                        cu =
                            Culture{..}

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


{-# INLINE reactor #-}
{-# INLINE reacts #-}
