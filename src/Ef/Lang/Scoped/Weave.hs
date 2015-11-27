{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Weave
    ( Weavable
    , weaver
    , Weaving
    , weave

    , Producer
    , Producer'
    , producer

    , Consumer
    , Consumer'
    , consumer

    , Pipe
    , pipe

    , Client
    , Client'
    , client

    , Server
    , Server'
    , server

    , Woven(..)
    , Effect
    , Effect'
    , woven

    , X


    , (<\\)
    , (\<\)
    , (~>)
    , (<~)
    , (/>/)
    , (//>)

    , (\\<)
    , (/</)
    , (>~)
    , (~<)
    , (\>\)
    , (>\\)

    , (<~<)
    , (~<<)
    , (>>~)
    , (>~>)

    , (<<+)
    , (<+<)
    , (<-<)
    , (>->)
    , (>+>)
    , (+>>)

    , cat
    , for
    ) where



import Ef.Core

import Control.Applicative
import Control.Monad
import Data.Binary
import Unsafe.Coerce



data Weaving k
  where

    FreshScope
        :: (Int -> k)
        -> Weaving k

    Request
        :: Int
        -> a'
        -> (a  -> Pattern fs m r)
        -> Weaving k

    Respond
        :: Int
        -> b
        -> (b' -> Pattern fs m r)
        -> Weaving k



data Weavable k
  where

    Weavable
        :: Int
        -> k
        -> Weavable k



instance Uses Weavable gs m
    => Binary (Attribute Weavable gs m)
  where

    get =
        return weaver



    put _ =
        pure ()



freshScope
    :: Is Weaving fs m
    => Pattern fs m Int

freshScope =
    self (FreshScope id)



weaver
    :: Uses Weavable fs m
    => Attribute Weavable fs m

weaver =
    Weavable 0 $ \fs ->
        let
          Weavable n k =
              view fs

          n' =
              succ n

        in
          n' `seq` pure $ fs .=
              Weavable n' k



getScope
    :: Is Weaving fs m
    => Pattern fs m a
    -> m Int

getScope (Step sym _) =
    case prj sym of

        Just x ->
            case x of

                Request i _ _ ->
                    return i

                Respond i _ _ ->
                    return i



instance Witnessing Weavable Weaving
  where

    witness p (Weavable i k) (FreshScope ik) =
        p k (ik i)



weave
    :: Is Weaving fs m
    => Effect fs m r
    -> Pattern fs m r

weave e =
    do
      scope <- freshScope
      rewrite scope $ runWoven e (\a' apl -> self (Request scope a' apl))
                                 (\b b'p -> self (Respond scope b b'p))

rewrite rewriteScope = go
  where

    go (Fail e) =
        Fail e

    go (Pure r) =
        Pure r

    go (M m) =
        M (fmap go m)

    go (Step sym bp) =
        let
          check currentScope scoped =
              if currentScope == rewriteScope then
                  scoped
              else
                  ignore

          ignore =
              Step sym (go . bp)

        in
          case prj sym of

              Just x  ->
                  case x of

                      Request currentScope a' _ ->
                          check currentScope $
                              closed (unsafeCoerce a')

                      Respond currentScope b _ ->
                          check currentScope $
                              closed (unsafeCoerce b)

              Nothing -> Step sym (go . bp)



instance Functor m
    => Functor (Woven fs a' a b' b m)
  where

    fmap f (Woven w) =
        Woven $ \up dn -> fmap f (w up dn)



instance Monad m
    => Applicative (Woven fs a' a b' b m)
  where

    pure a =
        Woven $ \_ _ -> pure a



    wf <*> wx =
        Woven $ \up dn -> rewriteAp up dn (runWoven wf up dn)
      where

        rewriteAp up dn =
            go
          where

            go (Fail err) =
                Fail err

            go (M m) =
                M (fmap go m)

            go (Step sym bp) =
                Step sym (go . bp)

            go (Pure f) =
                fmap f (runWoven wx (unsafeCoerce up)
                                    (unsafeCoerce dn)
                       )



    (*>) = (>>)



instance Monad m
    => Monad (Woven fs a' a b' b m)
  where

    return =
        pure



    r >>= rs =
        Woven $ \up dn ->
            do
              v <- runWoven r (unsafeCoerce up) (unsafeCoerce dn)
              runWoven (rs v) up dn



instance ( Monad m
         , Monoid r
         ) => Monoid (Woven fs a' a b' b m r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        Woven $ \up dn ->
            rewriteMappend up dn (runWoven w1 up dn)
      where

        rewriteMappend up dn = go
          where

            go (Fail err) =
                Fail err

            go (M m) =
                M (fmap go m)

            go (Step sym bp) =
                Step sym (go . bp)

            go (Pure result) =
                let
                  routine =
                      runWoven w2 (unsafeCoerce up) (unsafeCoerce dn)

                in
                  fmap (mappend result) routine



instance MonadPlus m
    => Alternative (Woven fs a' a b' b m)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus m
    => MonadPlus (Woven fs a' a b' b m)
  where

    mzero =
        Woven $ \_ _ -> lift_ mzero

    mplus w0 w1 =
        Woven $ \up dn ->
            let
              routine =
                  runWoven w0 (unsafeCoerce up) (unsafeCoerce dn)
            in
              rewriteMplus up dn routine
      where
        rewriteMplus up dn = go
          where

            go (Fail err) =
                Fail err

            go (Pure r) =
                Pure r

            go (Step sym bp) =
                Step sym (go . bp)

            go (M m) =
              let
                routine =
                    runWoven w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                M (fmap go m `mplus` return routine)



newtype X = X X



closed
    :: X
    -> a

closed (X x) =
    closed x



type Effect fs m r =
    Woven fs X () () X m r



type Producer b fs m r =
    Woven fs X () () b m r



producer
    :: forall fs m b r.
       Is Weaving fs m
    => (    (    b
              -> Pattern fs m ()
            )
         -> Pattern fs m r
       )
    -> Producer' b fs m r

producer f =
    Woven $ \_ dn ->
        do
          let
            scopedDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond scope b =
                self (Respond scope b Pure)

          i <- lift (getScope scopedDown)
          f (respond i)



type Consumer a fs m r =
    Woven fs () a () X m r



consumer
    :: forall fs m a r.
       Is Weaving fs m
    => (    Pattern fs m a
         -> Pattern fs m r
       )
    -> Consumer' a fs m r

consumer f =
    Woven $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request scope =
                self (Request scope () Pure)

          i <- lift (getScope scopedUp)
          f (request i)



type Pipe a b fs m r = Woven fs () a () b m r



pipe
    :: forall fs m a b x r.
       Is Weaving fs m
    => (    Pattern fs m a
         -> (    b
              -> Pattern fs m x
            )
         -> Pattern fs m r
       )
    -> Pipe a b fs m r

pipe f =
    Woven $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            request =
                self (Request i () Pure)

            respond b =
                self (Respond i b Pure)

          f request respond



type Client a' a fs m r =
    Woven fs a' a () X m r



client
    :: forall fs m a' a r.
       Is Weaving fs m
    => (    (    a'
              -> Pattern fs m a
            )
         -> Pattern fs m r
       )
    -> Client' a' a fs m r

client f =
    Woven $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            request a =
                self (Request i a Pure)

          f request



type Server b' b fs m r =
    Woven fs X () b' b m r



server
    :: forall fs m b' b r.
       Is Weaving fs m
    => (    (    b
              -> Pattern fs m b'
            )
         -> Pattern fs m r
       )
    -> Server' b' b fs m r

server f =
    Woven $ \_ dn ->
        do
         let
           scopedDown =
               dn (unsafeCoerce ()) (unsafeCoerce ())

         i <- lift (getScope scopedDown)
         let
           respond b' =
               self (Respond i b' Pure)

         f respond



newtype Woven fs a' a b' b m r =
    Woven
        {
          runWoven
              :: (forall x.
                      a'
                   -> (    a
                        -> Pattern fs m x
                      )
                   -> Pattern fs m x
                 )
              -> (forall x.
                      b
                   -> (    b'
                        -> Pattern fs m x
                      )
                   -> Pattern fs m x
                 )
             -> Pattern fs m r
        }



woven
    :: forall fs a a' b b' m r.
       Is Weaving fs m
    => (    (    a
              -> Pattern fs m a'
            )
         -> (    b'
              -> Pattern fs m b
            )
         -> Pattern fs m r
       )
    -> Woven fs a' a b' b m r

woven f =
    Woven $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            request a =
                self (Request i a Pure)

            respond b' =
                self (Respond i b' Pure)

          f request respond



type Effect' fs m r =
    forall x' x y' y.
    Woven fs x' x y' y m r



type Producer' b fs m r =
    forall x' x.
    Woven fs x' x () b m r



type Consumer' a fs m r =
    forall y' y.
    Woven fs () a y' y m r



type Server' b' b fs m r =
    forall x' x.
    Woven fs x' x b' b m r



type Client' a' a fs m r =
    forall y' y.
    Woven fs a' a y' y m r



--------------------------------------------------------------------------------
-- Respond; substitute yields with a function

cat
    :: Is Weaving fs m
    => Pipe a a fs m r

cat =
    pipe $ \awt yld ->
        forever (awt >>= yld)



infixl 3 //>
(//>)
    :: forall fs x' x b' b c' c m a'.
       Is Weaving fs m
    => Woven fs x' x b' b m a'
    -> (    b
         -> Woven fs x' x c' c m b'
       )
    -> Woven fs x' x c' c m a'

p0 //> fb =
    Woven $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            routine =
                runWoven p0 up (unsafeCoerce dn)

          substituteResponds fb i up dn routine



substituteResponds fb rewriteScope up dn =
    go
  where

    go (Fail err) =
        Fail err

    go (Pure r) =
        Pure r

    go (M m) =
        M (fmap go m)

    go (Step sym bp) =
        let
          check currentScope scoped =
              if currentScope == rewriteScope then
                  scoped
              else
                  ignore

          ignore =
              Step sym (go . bp)

        in
          case prj sym of

              Just x ->
                  case x of

                      Respond currentScope b _ ->
                          check currentScope $
                              do
                                let
                                  routine =
                                      runWoven (fb (unsafeCoerce b))
                                               (unsafeCoerce up)
                                               (unsafeCoerce dn)

                                res <- routine
                                let
                                  continue =
                                    bp (unsafeCoerce res)

                                go continue

                      _ ->
                          ignore

              _ ->
                  ignore



for
    :: Is Weaving fs m
    => Woven fs x' x b' b m a'
    -> (    b
         -> Woven fs x' x c' c m b'
       )
    -> Woven fs x' x c' c m a'

for =
    (//>)



infixr 3 <\\
(<\\)
    :: Is Weaving fs m
    => (    b
         -> Woven fs x' x c' c m b'
       )
    -> Woven fs x' x b' b m a'
    -> Woven fs x' x c' c m a'

f <\\ p =
    p //> f



infixl 4 \<\
(\<\)
    :: Is Weaving fs m
    => (    b
         -> Woven fs x' x c' c m b'
       )
    -> (    a
         -> Woven fs x' x b' b m a'
       )
    -> a
    -> Woven fs x' x c' c m a'

p1 \<\ p2 = p2 />/ p1



infixr 4 ~>
(~>)
    :: Is Weaving fs m
    => (    a
         -> Woven fs x' x b' b m a'
       )
    -> (    b
         -> Woven fs x' x c' c m b'
       )
    -> a
    -> Woven fs x' x c' c m a'

(~>) =
    (/>/)



infixl 4 <~
(<~)
    :: Is Weaving fs m
    => (    b
         -> Woven fs x' x c' c m b'
       )
    -> (    a
         -> Woven fs x' x b' b m a'
       )
    -> a
    -> Woven fs x' x c' c m a'

g <~ f =
    f ~> g



infixr 4 />/
(/>/)
    :: Is Weaving fs m
    => (    a
         -> Woven fs x' x b' b m a'
       )
    -> (    b
         -> Woven fs x' x c' c m b'
       )
    -> a
    -> Woven fs x' x c' c m a'

(fa />/ fb) a =
    fa a //> fb



--------------------------------------------------------------------------------
-- Request; substitute awaits with a function


infixr 4 >\\
(>\\)
    :: forall fs y' y a' a b' b m c.
       Is Weaving fs m
    => (    b'
         -> Woven fs a' a y' y m b
       )
    -> Woven fs b' b y' y m c
    -> Woven fs a' a y' y m c

fb' >\\ p0 =
    Woven $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            routine =
                runWoven p0 (unsafeCoerce up) dn

          substituteRequests fb' i up dn routine

substituteRequests fb' rewriteScope up dn p1 = go p1
  where

    go (Fail err) =
        Fail err

    go (Pure r) =
        Pure r

    go (M m) =
        M (fmap go m)

    go (Step sym bp) =
        let
          check currentScope scoped =
              if currentScope == rewriteScope then
                  scoped
              else
                  ignore

          ignore =
              Step sym (go . bp)

        in
          case prj sym of

              Just x ->
                  case x of

                      Request currentScope b' _ ->
                          check currentScope $
                              do
                                let
                                  routine =
                                      runWoven (fb' (unsafeCoerce b'))
                                               (unsafeCoerce up)
                                               (unsafeCoerce dn)

                                res <- routine
                                let
                                  continue =
                                      bp (unsafeCoerce res)

                                go continue

                      _ ->
                          ignore

              _ ->
                  ignore



infixr 5 /</
(/</)
    :: Is Weaving fs m
    => (    c'
         -> Woven fs b' b x' x m c
       )
    -> (    b'
         -> Woven fs a' a x' x m b
       )
    -> c'
    -> Woven fs a' a x' x m c

p1 /</ p2 =
    p2 \>\ p1



infixr 5 >~
(>~)
    :: Is Weaving fs m
    => Woven fs a' a y' y m b
    -> Woven fs () b y' y m c
    -> Woven fs a' a y' y m c

p1 >~ p2 =
    (\() -> p1) >\\ p2



infixl 5 ~<
(~<)
    :: Is Weaving fs m
    => Woven fs () b y' y m c
    -> Woven fs a' a y' y m b
    -> Woven fs a' a y' y m c

p2 ~< p1 =
    p1 >~ p2



infixl 5 \>\
(\>\)
    :: Is Weaving fs m
    => (    b'
         -> Woven fs a' a y' y m b
       )
    -> (    c'
         -> Woven fs b' b y' y m c
       )
    -> c'
    -> Woven fs a' a y' y m c

(fb' \>\ fc') c' =
    fb' >\\ fc' c'



infixl 4 \\<
(\\<)
    :: forall fs y' y a' a b' b m c.
       Is Weaving fs m
    => Woven fs b' b y' y m c
    -> (    b'
         -> Woven fs a' a y' y m b
       )
    -> Woven fs a' a y' y m c
p \\< f =
    f >\\ p



--------------------------------------------------------------------------------
-- Push; substitute responds with requests


infixl 7 >>~
(>>~)
    :: forall fs a' a b' b c' c m r. Is Weaving fs m
    => Woven fs a' a b' b m r
    -> (    b
         -> Woven fs b' b c' c m r
       )
    -> Woven fs a' a c' c m r
p0 >>~ fb0 =
    Woven $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          pushRewrite i up dn fb0 p0



pushRewrite rewriteScope up dn fb0 p0 =
    let
      upstream =
          runWoven p0 (unsafeCoerce up) (unsafeCoerce dn)

      downstream b =
          runWoven (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)

    in
      goLeft downstream upstream
  where

    goLeft fb = goLeft'
      where

        goLeft' (Pure r) =
            Pure r

        goLeft' (Fail err) =
            Fail err

        goLeft' (M m) =
            M (fmap goLeft' m)

        goLeft' (Step sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Step sym (goLeft' . bp)

            in
              case prj sym of

                Just x ->
                    case x of

                        Respond currentScope b _ ->
                            check currentScope $
                                goRight (unsafeCoerce bp)
                                        (fb (unsafeCoerce b))

                        _ ->
                            ignore
                _ ->
                    ignore


    goRight b'p = goRight'
      where
        goRight' (Fail err) =
            Fail err

        goRight' (M m) =
            M (fmap goRight' m)

        goRight' (Pure r) =
            Pure r

        goRight' (Step sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Step sym (goRight' . bp)
            in
              case prj sym of

                  Just x  ->
                      case x of

                          Request currentScope b' _ ->
                              check currentScope $
                                  goLeft (unsafeCoerce bp)
                                         (b'p (unsafeCoerce b'))

                          _ ->
                              ignore

                  _ ->
                      ignore




infixl 8 <~<
(<~<)
    :: Is Weaving fs m
    => (    b
         -> Woven fs b' b c' c m r
       )
    -> (    a
         -> Woven fs a' a b' b m r
       )
    -> a
    -> Woven fs a' a c' c m r

p1 <~< p2 =
    p2 >~> p1



infixr 8 >~>
(>~>)
    :: Is Weaving fs m
    => (    _a
          -> Woven fs a' a b' b m r
       )
    -> (    b
         -> Woven fs b' b c' c m r
       )
    -> _a
    -> Woven fs a' a c' c m r

(fa >~> fb) a =
    fa a >>~ fb



infixr 7 ~<<
(~<<)
    :: Is Weaving fs m
    => (    b
         -> Woven fs b' b c' c m r
       )
    -> Woven fs a' a b' b m r
    -> Woven fs a' a c' c m r

k ~<< p =
    p >>~ k



--------------------------------------------------------------------------------
-- Pull; substitute requests with responds


infixr 6 +>>
(+>>) :: forall fs m a' a b' b c' c r. Is Weaving fs m
      => (b' -> Woven fs a' a b' b m r)
      ->        Woven fs b' b c' c m r
      ->        Woven fs a' a c' c m r
fb' +>> p0 =
    Woven $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          pullRewrite i up dn fb' p0

pullRewrite rewriteScope up dn fb' p =
    let
      upstream b' =
          runWoven (fb' b') (unsafeCoerce up) (unsafeCoerce dn)

      downstream =
          runWoven p (unsafeCoerce up) (unsafeCoerce dn)

    in
      goRight upstream downstream
  where

    goRight fb'' =
        goRight'
      where

        goRight' (Fail err) =
            Fail err

        goRight' (M m) =
            M (fmap goRight' m)

        goRight' (Pure r) =
            Pure r

        goRight' (Step sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Step sym (goRight' . bp)
            in
              case prj sym of

                  Just x ->
                      case x of

                          Request currentScope b' _ ->
                              check currentScope $
                                  goLeft (unsafeCoerce bp)
                                         (fb'' (unsafeCoerce b'))

                          _ ->
                              ignore
                  _ ->
                      ignore


    goLeft bp = goLeft'
      where

        goLeft' (Fail err) =
            Fail err

        goLeft' (M m) =
            M (fmap goLeft' m)

        goLeft' (Pure r) =
            Pure r

        goLeft' (Step sym bp') =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Step sym (goLeft' . bp')

            in
              case prj sym of

                  Just x ->
                      case x of

                          Respond currentScope b _ ->
                              check currentScope $
                                  goRight (unsafeCoerce bp')
                                          (bp (unsafeCoerce b))

                          _ ->
                              ignore

                  _ ->
                      ignore




infixl 7 >->
(>->)
    :: Is Weaving fs m
    => Woven fs a' a () b m r
    -> Woven fs () b c' c m r
    -> Woven fs a' a c' c m r

p1 >-> p2 =
    (\() -> p1) +>> p2


infixr 7 <-<
(<-<)
    :: Is Weaving fs m
    => Woven fs () b c' c m r
    -> Woven fs a' a () b m r
    -> Woven fs a' a c' c m r

p2 <-< p1 =
    p1 >-> p2



infixr 7 <+<
(<+<)
    :: Is Weaving fs m
    => (    c'
         -> Woven fs b' b c' c m r
       )
    -> (    b'
         -> Woven fs a' a b' b m r
       )
    -> c'
    -> Woven fs a' a c' c m r

p1 <+< p2 =
    p2 >+> p1



infixl 7 >+>
(>+>)
    :: Is Weaving fs m
    => (    b'
         -> Woven fs a' a b' b m r
       )
    -> (    _c'
         -> Woven fs b' b c' c m r
       )
    -> _c'
    -> Woven fs a' a c' c m r

(fb' >+> fc') c' =
    fb' +>> fc' c'



infixl 6 <<+
(<<+)
    :: forall fs m a' a b' b c' c r. Is Weaving fs m
    => Woven fs b' b c' c m r
    -> (    b'
         -> Woven fs a' a b' b m r
       )
    -> Woven fs a' a c' c m r

p <<+ fb =
    fb +>> p



{-# RULES
    "(p //> f) //> g" forall p f g . (p //> f) //> g = p //> (\x -> f x //> g)

  ; "f >\\ (g >\\ p)" forall f g p . f >\\ (g >\\ p) = (\x -> f >\\ g x) >\\ p

  ; "(p >>~ f) >>~ g" forall p f g . (p >>~ f) >>~ g = p >>~ (\x -> f x >>~ g)

  ; "f +>> (g +>> p)" forall f g p . f +>> (g +>> p) = (\x -> f +>> g x) +>> p

  ; "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

  ; "f >~ (g >~ p)" forall f g p . f >~ (g >~ p) = (f >~ g) >~ p

  ; "p1 >-> (p2 >-> p3)" forall p1 p2 p3 .
        p1 >-> (p2 >-> p3) = (p1 >-> p2) >-> p3

  #-}

{-# INLINE freshScope #-}
{-# INLINE weaver #-}
{-# INLINE getScope #-}
{-# INLINE weave #-}
{-# INLINE closed #-}
{-# INLINE producer #-}
{-# INLINE consumer #-}
{-# INLINE pipe #-}
{-# INLINE client #-}
{-# INLINE server #-}
{-# INLINE woven #-}

{-# INLINE rewrite #-}
{-# INLINE substituteResponds #-}
{-# INLINE substituteRequests #-}
{-# INLINE pullRewrite #-}
{-# INLINE pushRewrite #-}


{-# INLINE (//>) #-}
{-# INLINE for #-}
{-# INLINE (<\\) #-}
{-# INLINE (\<\) #-}
{-# INLINE (~>) #-}
{-# INLINE (<~) #-}
{-# INLINE (/>/) #-}

{-# INLINE (>\\) #-}
{-# INLINE (/</) #-}
{-# INLINE (>~) #-}
{-# INLINE (~<) #-}
{-# INLINE (\>\) #-}
{-# INLINE (\\<) #-}

{-# INLINE (>>~) #-}
{-# INLINE (<~<) #-}
{-# INLINE (>~>) #-}
{-# INLINE (~<<) #-}

{-# INLINE (+>>) #-}
{-# INLINE (>->) #-}
{-# INLINE (<-<) #-}
{-# INLINE (<+<) #-}
{-# INLINE (>+>) #-}
{-# INLINE (<<+) #-}
