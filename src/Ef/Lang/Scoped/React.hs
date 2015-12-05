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



data Behavior a scope parent =
    Behavior
        {
          cultured
              :: Int

        , behavior
              :: IORef (a -> Pattern scope parent ())
        }



data Culture a scope parent =
    Culture
        {
          cultureId
              :: Int

        , culture
              :: IORef (a -> Pattern scope parent ())

        , behaviors
              :: IORef [Behavior a scope parent]
        }



data Reacting k
    = FreshScope (Int -> k)
    | forall a scope parent. NewCulture      Int (Culture a scope parent -> k)
    | forall a scope parent. NewBehavior     Int (Culture a scope parent) (a -> Pattern scope parent ()) (Behavior a scope parent -> k)
    | forall a scope parent. ModifyBehavior  Int (Behavior a scope parent) (a -> Pattern scope parent ()) k
    | forall a scope parent. TriggerBehavior Int (Behavior a scope parent) a k
    | forall a scope parent. TriggerCulture  Int (Culture a scope parent) a k
    | forall a scope parent. DestroyBehavior Int (Behavior a scope parent) k
    | forall a scope parent. DestroyCulture  Int (Culture a scope parent) k



data React scope parent =
    React
        {
          newC
              :: forall a.
                 Pattern scope parent (Culture a scope parent)

        , newB
              :: forall a.
                 Culture a scope parent
              -> (    a
                   -> Pattern scope parent ()
                 )
              -> Pattern scope parent (Behavior a scope parent)

        , modB
              :: forall a.
                 Behavior a scope parent
              -> (    a
                   -> Pattern scope parent ()
                 )
              -> Pattern scope parent ()

        , triggerB
              :: forall a.
                 Behavior a scope parent
              -> a
              -> Pattern scope parent ()

        , triggerC
              :: forall a.
                 Culture a scope parent
              -> a
              -> Pattern scope parent ()

        , destroyB
              :: forall a.
                 Behavior a scope parent
              -> Pattern scope parent ()

        , destroyC
              :: forall a.
                 Culture a scope parent
              -> Pattern scope parent ()
        }



data Reactable k =
    Reactable Int k



instance Uses Reactable attrs parent
    => Binary (Attribute Reactable attrs parent)
  where

    get =
        return reactor



    put _ =
        pure ()




reactor
    :: Uses Reactable attrs parent
    => Attribute Reactable attrs parent

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



reacts
    :: forall scope parent result.
       ( Is Reacting scope parent
       , Lift IO parent
       )
    => (    React scope parent
         -> Pattern scope parent result
       ) -> Pattern scope parent result

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

        go :: forall b. Int -> Pattern scope parent b -> Pattern scope parent b
        go c = go'
          where

            go' :: forall c. Pattern scope parent c -> Pattern scope parent c
            go' (Fail err) =
                Fail err

            go' (Pure res) =
                Pure res

            go' (Super m) =
                Super (fmap go' m)

            go' (Send sym bp) =
                let
                  check currentScope scoped =
                      if currentScope == rewriteScope then
                          scoped
                      else
                          ignore

                  ignore =
                      Send sym (go' . bp)

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
                    compile' [] = return () :: Pattern scope parent ()
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
                        compile' [] = return () :: Pattern scope parent ()
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
              writeIORef (behavior b) (const (return ()) :: forall d. d -> Pattern scope parent ())
            destroyCulture cu = do
              writeIORef (culture cu) (const (return ()) :: forall d. d -> Pattern scope parent ())
              bs <- readIORef (behaviors cu)
              mapM_ destroyBehavior bs


{-# INLINE reactor #-}
{-# INLINE reacts #-}
