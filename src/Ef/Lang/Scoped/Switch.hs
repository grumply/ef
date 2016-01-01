-- | An embedding of Gabriel Gonzalez's Pipes library with 
-- slight modifications for nesting and scoping and minor renaming.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Scoped.Switch
    ( Switchable
    , switcher
    , Switching
    , switch

    , Producer
    , Producer'
    , producer

    , Consumer
    , Consumer'
    , consumer

    , Line
    , line

    , Client
    , Client'
    , client

    , Server
    , Server'
    , server

    , Switched(..)
    , Effect
    , Effect'
    , switched

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



data Switching k
  where

    FreshScope
        :: (Int -> k)
        -> Switching k

    Request
        :: Int
        -> a'
        -> (a  -> Pattern scope parent r)
        -> Switching k

    Respond
        :: Int
        -> b
        -> (b' -> Pattern scope parent r)
        -> Switching k



data Switchable k
  where

    Switchable
        :: Int
        -> k
        -> Switchable k



instance Uses Switchable attrs parent
    => Binary (Attribute Switchable attrs parent)
  where

    get =
        return switcher



    put _ =
        pure ()



freshScope
    :: Is Switching scope parent
    => Pattern scope parent Int

freshScope =
    self (FreshScope id)



switcher
    :: Uses Switchable attrs parent
    => Attribute Switchable attrs parent

switcher =
    Switchable 0 $ \fs ->
        let
          Switchable n k =
              view fs

          n' =
              succ n

        in
          n' `seq` pure $ fs .=
              Switchable n' k



getScope
    :: Is Switching scope parent
    => Pattern scope parent a
    -> parent Int

getScope (Send symbol _) =
    case prj symbol of

        Just x ->
            case x of

                Request i _ _ ->
                    return i

                Respond i _ _ ->
                    return i



instance Witnessing Switchable Switching
  where

    witness p (Switchable i k) (FreshScope ik) =
        p k (ik i)



switch
    :: Is Switching scope parent
    => Effect scope parent r
    -> Pattern scope parent r

switch e =
    do
      scope <- freshScope
      rewrite scope $
          runSwitched e
              (\a' apl -> self (Request scope a' apl))
              (\b b'p -> self (Respond scope b b'p))



rewrite rewriteScope = go
  where

    go (Fail e) =
        Fail e

    go (Pure r) =
        Pure r

    go (Super m) =
        Super (fmap go m)

    go (Send symbol k) =
        let
          check currentScope scoped =
              if currentScope == rewriteScope then
                  scoped
              else
                  ignore

          ignore =
              Send symbol (go . k)

        in
          case prj symbol of

              Just x  ->
                  case x of

                      Request currentScope a' _ ->
                          check currentScope $
                              closed (unsafeCoerce a')

                      Respond currentScope b _ ->
                          check currentScope $
                              closed (unsafeCoerce b)

              Nothing -> Send symbol (go . k)



instance Functor m
    => Functor (Switched fs a' a b' b m)
  where

    fmap f (Switched w) =
        Switched $ \up dn -> fmap f (w up dn)



instance Monad m
    => Applicative (Switched fs a' a b' b m)
  where

    pure a =
        Switched $ \_ _ -> pure a



    wf <*> wx =
        Switched $ \up dn -> rewriteAp up dn (runSwitched wf up dn)
      where

        rewriteAp up dn =
            go
          where

            go (Fail err) =
                Fail err

            go (Super m) =
                Super (fmap go m)

            go (Send sym k) =
                Send sym (go . k)

            go (Pure f) =
                fmap f (runSwitched wx (unsafeCoerce up)
                                    (unsafeCoerce dn)
                       )



    (*>) = (>>)



instance Monad m
    => Monad (Switched fs a' a b' b m)
  where

    return =
        pure



    r >>= rs =
        Switched $ \up dn ->
            do
              v <- runSwitched r (unsafeCoerce up) (unsafeCoerce dn)
              runSwitched (rs v) up dn



instance ( Monad m
         , Monoid r
         ) => Monoid (Switched fs a' a b' b m r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        Switched $ \up dn ->
            rewriteMappend up dn (runSwitched w1 up dn)
      where

        rewriteMappend up dn = go
          where

            go (Fail err) =
                Fail err

            go (Super m) =
                Super (fmap go m)

            go (Send sym bp) =
                Send sym (go . bp)

            go (Pure result) =
                let
                  routine =
                      runSwitched w2 (unsafeCoerce up) (unsafeCoerce dn)

                in
                  fmap (mappend result) routine



instance MonadPlus m
    => Alternative (Switched fs a' a b' b m)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus m
    => MonadPlus (Switched fs a' a b' b m)
  where

    mzero =
        Switched $ \_ _ -> lift_ mzero

    mplus w0 w1 =
        Switched $ \up dn ->
            let
              routine =
                  runSwitched w0 (unsafeCoerce up) (unsafeCoerce dn)
            in
              rewriteMplus up dn routine
      where
        rewriteMplus up dn = go
          where

            go (Fail err) =
                Fail err

            go (Pure r) =
                Pure r

            go (Send sym bp) =
                Send sym (go . bp)

            go (Super m) =
              let
                routine =
                    runSwitched w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                Super (fmap go m `mplus` return routine)



newtype X = X X



closed
    :: X
    -> a

closed (X x) =
    closed x



type Effect fs m r =
    Switched fs X () () X m r



type Producer b fs m r =
    Switched fs X () () b m r



producer
    :: forall fs m b r.
       Is Switching fs m
    => (    (    b
              -> Pattern fs m ()
            )
         -> Pattern fs m r
       )
    -> Producer' b fs m r

producer f =
    Switched $ \_ dn ->
        do
          let
            scopedDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond scope b =
                self (Respond scope b Pure)

          i <- lift (getScope scopedDown)
          f (respond i)



type Consumer a fs m r =
    Switched fs () a () X m r



consumer
    :: forall fs m a r.
       Is Switching fs m
    => (    Pattern fs m a
         -> Pattern fs m r
       )
    -> Consumer' a fs m r

consumer f =
    Switched $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request scope =
                self (Request scope () Pure)

          i <- lift (getScope scopedUp)
          f (request i)



type Line a b fs m r = Switched fs () a () b m r



line
    :: forall fs m a b x r.
       Is Switching fs m
    => (    Pattern fs m a
         -> (    b
              -> Pattern fs m x
            )
         -> Pattern fs m r
       )
    -> Line a b fs m r

line f =
    Switched $ \up _ ->
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
    Switched fs a' a () X m r



client
    :: forall fs m a' a r.
       Is Switching fs m
    => (    (    a'
              -> Pattern fs m a
            )
         -> Pattern fs m r
       )
    -> Client' a' a fs m r

client f =
    Switched $ \up _ ->
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
    Switched fs X () b' b m r



server
    :: forall fs m b' b r.
       Is Switching fs m
    => (    (    b
              -> Pattern fs m b'
            )
         -> Pattern fs m r
       )
    -> Server' b' b fs m r

server f =
    Switched $ \_ dn ->
        do
         let
           scopedDown =
               dn (unsafeCoerce ()) (unsafeCoerce ())

         i <- lift (getScope scopedDown)
         let
           respond b' =
               self (Respond i b' Pure)

         f respond



newtype Switched fs a' a b' b m r =
    Switched
        {
          runSwitched
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



switched
    :: forall fs a a' b b' m r.
       Is Switching fs m
    => (    (    a
              -> Pattern fs m a'
            )
         -> (    b'
              -> Pattern fs m b
            )
         -> Pattern fs m r
       )
    -> Switched fs a' a b' b m r

switched f =
    Switched $ \up _ ->
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
    Switched fs x' x y' y m r



type Producer' b fs m r =
    forall x' x.
    Switched fs x' x () b m r



type Consumer' a fs m r =
    forall y' y.
    Switched fs () a y' y m r



type Server' b' b fs m r =
    forall x' x.
    Switched fs x' x b' b m r



type Client' a' a fs m r =
    forall y' y.
    Switched fs a' a y' y m r



--------------------------------------------------------------------------------
-- Respond; substitute yields with a function

cat
    :: Is Switching fs m
    => Line a a fs m r

cat =
    line $ \awt yld ->
        forever (awt >>= yld)



infixl 3 //>
(//>)
    :: forall fs x' x b' b c' c m a'.
       Is Switching fs m
    => Switched fs x' x b' b m a'
    -> (    b
         -> Switched fs x' x c' c m b'
       )
    -> Switched fs x' x c' c m a'

p0 //> fb =
    Switched $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            routine =
                runSwitched p0 up (unsafeCoerce dn)

          substituteResponds fb i up dn routine



substituteResponds fb rewriteScope up dn =
    go
  where

    go (Fail err) =
        Fail err

    go (Pure r) =
        Pure r

    go (Super m) =
        Super (fmap go m)

    go (Send sym bp) =
        let
          check currentScope scoped =
              if currentScope == rewriteScope then
                  scoped
              else
                  ignore

          ignore =
              Send sym (go . bp)

        in
          case prj sym of

              Just x ->
                  case x of

                      Respond currentScope b _ ->
                          check currentScope $
                              do
                                let
                                  routine =
                                      runSwitched (fb (unsafeCoerce b))
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
    :: Is Switching fs m
    => Switched fs x' x b' b m a'
    -> (    b
         -> Switched fs x' x c' c m b'
       )
    -> Switched fs x' x c' c m a'

for =
    (//>)



infixr 3 <\\
(<\\)
    :: Is Switching fs m
    => (    b
         -> Switched fs x' x c' c m b'
       )
    -> Switched fs x' x b' b m a'
    -> Switched fs x' x c' c m a'

f <\\ p =
    p //> f



infixl 4 \<\
(\<\)
    :: Is Switching fs m
    => (    b
         -> Switched fs x' x c' c m b'
       )
    -> (    a
         -> Switched fs x' x b' b m a'
       )
    -> a
    -> Switched fs x' x c' c m a'

p1 \<\ p2 = p2 />/ p1



infixr 4 ~>
(~>)
    :: Is Switching fs m
    => (    a
         -> Switched fs x' x b' b m a'
       )
    -> (    b
         -> Switched fs x' x c' c m b'
       )
    -> a
    -> Switched fs x' x c' c m a'

(~>) =
    (/>/)



infixl 4 <~
(<~)
    :: Is Switching fs m
    => (    b
         -> Switched fs x' x c' c m b'
       )
    -> (    a
         -> Switched fs x' x b' b m a'
       )
    -> a
    -> Switched fs x' x c' c m a'

g <~ f =
    f ~> g



infixr 4 />/
(/>/)
    :: Is Switching fs m
    => (    a
         -> Switched fs x' x b' b m a'
       )
    -> (    b
         -> Switched fs x' x c' c m b'
       )
    -> a
    -> Switched fs x' x c' c m a'

(fa />/ fb) a =
    fa a //> fb



--------------------------------------------------------------------------------
-- Request; substitute awaits with a function


infixr 4 >\\
(>\\)
    :: forall fs y' y a' a b' b m c.
       Is Switching fs m
    => (    b'
         -> Switched fs a' a y' y m b
       )
    -> Switched fs b' b y' y m c
    -> Switched fs a' a y' y m c

fb' >\\ p0 =
    Switched $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            routine =
                runSwitched p0 (unsafeCoerce up) dn

          substituteRequests fb' i up dn routine

substituteRequests fb' rewriteScope up dn p1 = go p1
  where

    go (Fail err) =
        Fail err

    go (Pure r) =
        Pure r

    go (Super m) =
        Super (fmap go m)

    go (Send sym bp) =
        let
          check currentScope scoped =
              if currentScope == rewriteScope then
                  scoped
              else
                  ignore

          ignore =
              Send sym (go . bp)

        in
          case prj sym of

              Just x ->
                  case x of

                      Request currentScope b' _ ->
                          check currentScope $
                              do
                                let
                                  routine =
                                      runSwitched (fb' (unsafeCoerce b'))
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
    :: Is Switching fs m
    => (    c'
         -> Switched fs b' b x' x m c
       )
    -> (    b'
         -> Switched fs a' a x' x m b
       )
    -> c'
    -> Switched fs a' a x' x m c

p1 /</ p2 =
    p2 \>\ p1



infixr 5 >~
(>~)
    :: Is Switching fs m
    => Switched fs a' a y' y m b
    -> Switched fs () b y' y m c
    -> Switched fs a' a y' y m c

p1 >~ p2 =
    (\() -> p1) >\\ p2



infixl 5 ~<
(~<)
    :: Is Switching fs m
    => Switched fs () b y' y m c
    -> Switched fs a' a y' y m b
    -> Switched fs a' a y' y m c

p2 ~< p1 =
    p1 >~ p2



infixl 5 \>\
(\>\)
    :: Is Switching fs m
    => (    b'
         -> Switched fs a' a y' y m b
       )
    -> (    c'
         -> Switched fs b' b y' y m c
       )
    -> c'
    -> Switched fs a' a y' y m c

(fb' \>\ fc') c' =
    fb' >\\ fc' c'



infixl 4 \\<
(\\<)
    :: forall fs y' y a' a b' b m c.
       Is Switching fs m
    => Switched fs b' b y' y m c
    -> (    b'
         -> Switched fs a' a y' y m b
       )
    -> Switched fs a' a y' y m c
p \\< f =
    f >\\ p



--------------------------------------------------------------------------------
-- Push; substitute responds with requests


infixl 7 >>~
(>>~)
    :: forall fs a' a b' b c' c m r. Is Switching fs m
    => Switched fs a' a b' b m r
    -> (    b
         -> Switched fs b' b c' c m r
       )
    -> Switched fs a' a c' c m r
p0 >>~ fb0 =
    Switched $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          pushRewrite i up dn fb0 p0



pushRewrite rewriteScope up dn fb0 p0 =
    let
      upstream =
          runSwitched p0 (unsafeCoerce up) (unsafeCoerce dn)

      downstream b =
          runSwitched (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)

    in
      goLeft downstream upstream
  where

    goLeft fb = goLeft'
      where

        goLeft' (Pure r) =
            Pure r

        goLeft' (Fail err) =
            Fail err

        goLeft' (Super m) =
            Super (fmap goLeft' m)

        goLeft' (Send sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Send sym (goLeft' . bp)

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

        goRight' (Super m) =
            Super (fmap goRight' m)

        goRight' (Pure r) =
            Pure r

        goRight' (Send sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Send sym (goRight' . bp)
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
    :: Is Switching fs m
    => (    b
         -> Switched fs b' b c' c m r
       )
    -> (    a
         -> Switched fs a' a b' b m r
       )
    -> a
    -> Switched fs a' a c' c m r

p1 <~< p2 =
    p2 >~> p1



infixr 8 >~>
(>~>)
    :: Is Switching fs m
    => (    _a
          -> Switched fs a' a b' b m r
       )
    -> (    b
         -> Switched fs b' b c' c m r
       )
    -> _a
    -> Switched fs a' a c' c m r

(fa >~> fb) a =
    fa a >>~ fb



infixr 7 ~<<
(~<<)
    :: Is Switching fs m
    => (    b
         -> Switched fs b' b c' c m r
       )
    -> Switched fs a' a b' b m r
    -> Switched fs a' a c' c m r

k ~<< p =
    p >>~ k



--------------------------------------------------------------------------------
-- Pull; substitute requests with responds


infixr 6 +>>
(+>>) :: forall fs m a' a b' b c' c r. Is Switching fs m
      => (b' -> Switched fs a' a b' b m r)
      ->        Switched fs b' b c' c m r
      ->        Switched fs a' a c' c m r
fb' +>> p0 =
    Switched $ \up dn ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          pullRewrite i up dn fb' p0

pullRewrite rewriteScope up dn fb' p =
    let
      upstream b' =
          runSwitched (fb' b') (unsafeCoerce up) (unsafeCoerce dn)

      downstream =
          runSwitched p (unsafeCoerce up) (unsafeCoerce dn)

    in
      goRight upstream downstream
  where

    goRight fb'' =
        goRight'
      where

        goRight' (Fail err) =
            Fail err

        goRight' (Super m) =
            Super (fmap goRight' m)

        goRight' (Pure r) =
            Pure r

        goRight' (Send sym bp) =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Send sym (goRight' . bp)
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

        goLeft' (Super m) =
            Super (fmap goLeft' m)

        goLeft' (Pure r) =
            Pure r

        goLeft' (Send sym bp') =
            let
              check currentScope scoped =
                  if currentScope == rewriteScope then
                      scoped
                  else
                      ignore

              ignore =
                  Send sym (goLeft' . bp')

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
    :: Is Switching fs m
    => Switched fs a' a () b m r
    -> Switched fs () b c' c m r
    -> Switched fs a' a c' c m r

p1 >-> p2 =
    (\() -> p1) +>> p2


infixr 7 <-<
(<-<)
    :: Is Switching fs m
    => Switched fs () b c' c m r
    -> Switched fs a' a () b m r
    -> Switched fs a' a c' c m r

p2 <-< p1 =
    p1 >-> p2



infixr 7 <+<
(<+<)
    :: Is Switching fs m
    => (    c'
         -> Switched fs b' b c' c m r
       )
    -> (    b'
         -> Switched fs a' a b' b m r
       )
    -> c'
    -> Switched fs a' a c' c m r

p1 <+< p2 =
    p2 >+> p1



infixl 7 >+>
(>+>)
    :: Is Switching fs m
    => (    b'
         -> Switched fs a' a b' b m r
       )
    -> (    _c'
         -> Switched fs b' b c' c m r
       )
    -> _c'
    -> Switched fs a' a c' c m r

(fb' >+> fc') c' =
    fb' +>> fc' c'



infixl 6 <<+
(<<+)
    :: forall fs m a' a b' b c' c r. Is Switching fs m
    => Switched fs b' b c' c m r
    -> (    b'
         -> Switched fs a' a b' b m r
       )
    -> Switched fs a' a c' c m r

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
{-# INLINE switcher #-}
{-# INLINE getScope #-}
{-# INLINE switch #-}
{-# INLINE closed #-}
{-# INLINE producer #-}
{-# INLINE consumer #-}
{-# INLINE line #-}
{-# INLINE client #-}
{-# INLINE server #-}
{-# INLINE switched #-}

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
