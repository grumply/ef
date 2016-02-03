{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Ef.Lang.Scoped.Fiber.Lexemes
    ( Fiber(..)
    , Fibers(..)
    , fibers
    , module Ef.Lang.Scoped.Fiber.Lexicon
    ) where



import Ef.Core.Narrative
import Ef.Core.Narrative.Exception
import Ef.Lang.IO
import Ef.Lang.Scoped.Fiber.Lexicon

import Data.IORef
import Unsafe.Coerce

data Fibers lexicon environment =
    Fibers
        {
          fork
              :: forall status result.
                 (    Ops lexicon environment status result
                   -> Narrative lexicon environment result
                 )
              -> Narrative lexicon environment (Operation status result)

        , await
              :: forall status result.
                 Operation status result
              -> Narrative lexicon environment (Status status result)

        , focus
              :: forall focusResult.
                 Narrative lexicon environment focusResult
              -> Narrative lexicon environment focusResult

        , yield
              :: Narrative lexicon environment ()

        , chunk
              :: forall chunkResult.
                 Int
              -> Narrative lexicon environment chunkResult
              -> Narrative lexicon environment chunkResult
        }





fibers
    :: ( Knows Fiber lexicon environment
       , Lift IO environment
       )
    => (    Fibers lexicon environment
         -> Narrative lexicon environment a
       )
    -> Narrative lexicon environment a

fibers f =
    do
        lexicon <- say (FreshScope id)
        let
            newOp =
                Operation <$> newIORef (Running Nothing)

            root =
                f Fibers
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
                                        say (Fork lexicon op (p (ops op)))

                      , await =
                          \(Operation op) ->
                                 let
                                     awaiting =
                                         do
                                             status <- io (readIORef op)
                                             case status of

                                                 Running _ ->
                                                     do
                                                         say (Yield lexicon)
                                                         awaiting

                                                 _ ->
                                                     return status

                                  in
                                      awaiting

                      , focus =
                          \block ->
                              say (Focus lexicon block)

                      , yield =
                            say (Yield lexicon)

                      , chunk =
                            \chunking block ->
                                let
                                    chunked =
                                        go chunking block

                                    focused =
                                        say (Focus lexicon chunked)

                                    go !n (Return result) =
                                        Return result

                                    go n (Fail exception) =
                                        Fail exception

                                    go 1 (Super sup) =
                                        do
                                            say (Yield lexicon)
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

                                    go 1 (Say symbol k) =
                                        do
                                            say (Yield lexicon)
                                            Say symbol (go chunking . k)

                                    go n (Say symbol k) =
                                        let
                                            continue value =
                                                go newN (k value)

                                            newN =
                                                n - 1

                                        in
                                            Say symbol continue

                                in
                                    focused
                      }

        rootOp <- io newOp
        rewrite lexicon (unsafeCoerce rootOp) [unsafeCoerce (root,rootOp)]



rewrite lexicon (Operation rootOp) =
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

                go (Return result) =
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

                go (Say symbol k) =
                    let
                        check currentLexicon continue =
                            if currentLexicon == lexicon then
                                continue
                            else
                                ignore

                        ignore =
                            Say symbol $ \intermediate ->
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

                                    Fork currentLexicon childOp child ->
                                        check currentLexicon $
                                            let
                                                newAcc =
                                                    (k $ unsafeCoerce childOp,op):acc

                                                newFibers =
                                                    unsafeCoerce (child,childOp):fibers

                                            in
                                                withFibers newAcc newFibers

                                    Yield currentLexicon ->
                                        let
                                            newAcc =
                                                (k $ unsafeCoerce (),op):acc

                                        in
                                            withFibers newAcc fibers


                                    Focus currentLexicon block ->
                                        check currentLexicon $
                                            runFocus (unsafeCoerce k) (unsafeCoerce block)

                                    _ ->
                                        ignore

                runFocus focusK =
                    withNew []
                    where

                        withNew new (Return atomicResult) =
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

                        withNew new (Say symbol k) =
                            let
                               check currentLexicon continue =
                                   if currentLexicon == lexicon then
                                       continue
                                   else
                                       ignore

                               ignore =
                                   let
                                       continue intermediate =
                                           withNew new (k intermediate)

                                   in
                                       Say symbol continue

                            in
                                case prj symbol of

                                    Nothing ->
                                        ignore

                                    Just x ->
                                        case x of

                                            Fork currentLexicon childOp child ->
                                                check currentLexicon $
                                                    let
                                                        newNew =
                                                            unsafeCoerce (child,childOp):new

                                                    in
                                                        withNew newNew (k $ unsafeCoerce childOp)

                                            Yield currentLexicon ->
                                                check currentLexicon $
                                                    let
                                                        continue =
                                                            say (Focus currentLexicon $ k $ unsafeCoerce ())

                                                        refocus =
                                                            do
                                                                focusResult <- continue
                                                                focusK $ unsafeCoerce focusResult

                                                        newAcc =
                                                            (refocus,op):acc

                                                    in
                                                        withFibers newAcc fibers

                                            Focus currentLexicon block ->
                                                check currentLexicon $
                                                    withNew new $
                                                        do
                                                            intermediate <- unsafeCoerce block
                                                            k $ unsafeCoerce intermediate

                                            _ ->
                                                ignore


{-# INLINE rewrite #-}
{-# INLINE fibers #-}
