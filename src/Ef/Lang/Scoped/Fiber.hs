{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Ef.Lang.Scoped.Fiber
    ( Fibering
    , fibers
    , Fiberable
    , fiberer
    , Fiber(..)
    , Ops(..)
    ) where



import Ef.Core
import Ef.Lang.IO
import Ef.Data.Queue

import Data.Binary
import Data.IORef
import Unsafe.Coerce



data Fibering k
  where

    Fork
        :: Int
        -> Operation status result
        -> Pattern scope parent result
        -> Fibering k

    Yield
        :: Int
        -> Fibering k

    Focus
        :: Int
        -> Pattern scope parent result
        -> Fibering k

    FreshScope
        :: (Int -> k)
        -> Fibering k



data Status status result
  where

    Running
        :: Maybe status
        -> Status status result

    Failed
        :: SomeException
        -> Status status result

    Done
        :: result
        -> Status status result



data Operation status result
  where

    Operation
        :: IORef (Status status result)
        -> Operation status result



query
    :: ( Lift IO parent
       , Monad parent
       )
    => Operation status result
    -> Pattern scope parent (Status status result)

query (Operation op) =
    io (readIORef op)



data Ops scope parent status result =
    Ops
        {
          notify
              :: status
              -> Pattern scope parent ()

        , supplement
              :: (    Maybe status
                   -> Maybe status
                 )
              -> Pattern scope parent ()
        }


data Fiber scope parent =
    Fiber
        {
          fork
              :: forall status result.
                 (    Ops scope parent status result
                   -> Pattern scope parent result
                 )
              -> Pattern scope parent (Operation status result)

        , await
              :: forall status result.
                 Operation status result
              -> Pattern scope parent (Status status result)

        , focus
              :: forall focusResult.
                 Pattern scope parent focusResult
              -> Pattern scope parent focusResult

        , yield
              :: Pattern scope parent ()

        , chunk
              :: forall chunkResult.
                 Int
              -> Pattern scope parent chunkResult
              -> Pattern scope parent chunkResult
        }



data Fiberable k
  where

    Fiberable
        :: Int
        -> k
        -> Fiberable k



instance Uses Fiberable attrs parent
    => Binary (Attribute Fiberable attrs parent)
  where

    get =
        return fiberer



    put _ =
        pure ()



fiberer
    :: Uses Fiberable attrs parent
    => Attribute Fiberable attrs parent

fiberer = Fiberable 0 $ \fs ->
    let
      Fiberable i k =
          view fs

      i' =
          succ i

    in
      i' `seq` pure $ fs .=
          Fiberable i' k



-- | Attribute/Symbol Symmetry

instance Fiberable `Witnessing` Fibering
  where

    witness use (Fiberable i k) (FreshScope ik) =
        use k (ik i)



-- | Local Scoping Construct + Substitution

fibers
    :: ( Is Fibering scope parent
       , Lift IO parent
       )
    => (    Fiber scope parent
         -> Pattern scope parent a
       )
    -> Pattern scope parent a

fibers f =
    do
        scope <- self (FreshScope id)
        let
            newOp =
                Operation <$> newIORef (Running Nothing)

            root =
                f Fiber
                      {
                        fork =
                            \p ->
                                let
                                    ops (Operation op) =
                                        Ops
                                            {
                                              notify =
                                                  \status ->
                                                      let
                                                          running =
                                                              Running (Just status)

                                                      in
                                                          io (writeIORef op running)

                                            , supplement =
                                                  \supp ->
                                                      let
                                                          modify (Running x) =
                                                              Running (supp x)

                                                       in
                                                          io (modifyIORef op modify)
                                            }

                                    newOp =
                                        Operation <$> newIORef (Running Nothing)

                                in
                                    do
                                        op <- io newOp
                                        self (Fork scope op (p (ops op)))

                      , await =
                          \(Operation op) ->
                                 let
                                     awaiting =
                                         do
                                             status <- io (readIORef op)
                                             case status of

                                                 Running _ ->
                                                     do
                                                         self (Yield scope)
                                                         awaiting

                                                 _ ->
                                                     return status

                                  in
                                      awaiting

                      , focus =
                          \block ->
                              self (Focus scope block)

                      , yield =
                            self (Yield scope)

                      , chunk =
                            \chunking block ->
                                let
                                    chunked =
                                        go chunking block

                                    focused =
                                        self (Focus scope chunked)

                                    go !n (Pure result) =
                                        Pure result

                                    go n (Fail exception) =
                                        Fail exception

                                    go 1 (Super sup) =
                                        do
                                            self (Yield scope)
                                            let
                                                restart =
                                                    go chunking
                                                    
                                            Super (fmap restart sup)
                                            
                                    go n (Super sup) =
                                        let
                                            continue =
                                                go (n - 1)

                                        in
                                            Super (fmap continue sup)

                                    go 1 (Send symbol k) =
                                        do
                                            self (Yield scope)
                                            Send symbol (go chunking . k)

                                    go n (Send symbol k) =
                                        let
                                            continue value =
                                                go newN (k value)

                                            newN =
                                                n - 1

                                        in
                                            Send symbol continue

                                in
                                    focused
                      }

        rootOp <- io newOp
        rewrite scope (unsafeCoerce rootOp) [unsafeCoerce (root,rootOp)]



rewrite scope (Operation rootOp) =
    withFibers []
    where

        withFibers [] [] =
            do
                result <- io (readIORef rootOp)
                case result of

                    Failed exception ->
                        throw exception

                    ~(Done result) ->
                        return result

        withFibers acc [] =
            withFibers [] (reverse acc)

        withFibers acc ( (fiber,op@(Operation operation)) : fibers ) =
            go fiber
            where

                go (Pure result) =
                    let
                        finish =
                            writeIORef operation (Done result)

                    in
                        do
                            io finish
                            withFibers acc fibers

                go (Fail exception) =
                    let
                        fail =
                            writeIORef operation (Failed exception)

                    in
                        do
                            io fail
                            withFibers acc fibers

                go (Super sup) =
                    Super (fmap go sup)

                go (Send symbol k) =
                    let
                        check currentScope continue =
                            if currentScope == scope then
                                continue
                            else
                                ignore

                        ignore =
                            Send symbol $ \intermediate ->
                                let
                                    continue =
                                        k intermediate

                                    ran =
                                        (continue,op)

                                    newAcc =
                                         ran:acc

                                in
                                    withFibers newAcc fibers

                    in
                        case prj symbol of

                            Nothing ->
                                ignore

                            Just x ->
                                case x of

                                    Fork currentScope childOp child ->
                                        check currentScope $
                                            let
                                                newAcc =
                                                    (k $ unsafeCoerce childOp,op):acc

                                                newFibers =
                                                    unsafeCoerce (child,childOp):fibers

                                            in
                                                withFibers newAcc newFibers

                                    Yield currentScope ->
                                        let
                                            newAcc =
                                                (k $ unsafeCoerce (),op):acc

                                        in
                                            withFibers newAcc fibers


                                    Focus currentScope block ->
                                        check currentScope $
                                            runFocus (unsafeCoerce k) (unsafeCoerce block)

                                    _ ->
                                        ignore

                runFocus focusK =
                    withNew []
                    where

                        withNew new (Pure atomicResult) =
                            let
                                newAcc =
                                    (focusK $ unsafeCoerce atomicResult,op):acc

                            in
                                withFibers newAcc (new ++ fibers)

                        withNew new (Fail exception) =
                            let
                                fail =
                                    writeIORef operation (Failed exception)

                            in
                                do
                                    io fail
                                    withFibers acc (new ++ fibers)

                        withNew new (Super sup) =
                            let
                                continue =
                                    withNew new

                            in
                                Super (fmap continue sup)

                        withNew new (Send symbol k) =
                            let
                               check currentScope continue =
                                   if currentScope == scope then
                                       continue
                                   else
                                       ignore

                               ignore =
                                   let
                                       continue intermediate =
                                           withNew new (k intermediate)

                                   in
                                       Send symbol continue

                            in
                                case prj symbol of

                                    Nothing ->
                                        ignore

                                    Just x ->
                                        case x of

                                            Fork currentScope childOp child ->
                                                check currentScope $
                                                    let
                                                        newNew =
                                                            unsafeCoerce (child,childOp):new

                                                    in
                                                        withNew newNew (k $ unsafeCoerce childOp)

                                            Yield currentScope ->
                                                check currentScope $
                                                    let
                                                        continue =
                                                            self (Focus currentScope $ k $ unsafeCoerce ())

                                                        refocus =
                                                            do
                                                                focusResult <- continue
                                                                focusK $ unsafeCoerce focusResult

                                                        newAcc =
                                                            (refocus,op):acc

                                                    in
                                                        withFibers newAcc fibers

                                            Focus currentScope block ->
                                                check currentScope $
                                                    withNew new $
                                                        do
                                                            intermediate <- unsafeCoerce block
                                                            k $ unsafeCoerce intermediate

                                            _ ->
                                                ignore

-- | Inlines

{-# INLINE rewrite #-}
{-# INLINE fiberer #-}
{-# INLINE fibers #-}
