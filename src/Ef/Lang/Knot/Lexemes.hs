{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Knot.Lexemes
    ( Knots(..)
    , linearize

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

    , Knotted(..)
    , Effect
    , Effect'
    , knotted

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



import Ef.Core.Narrative

import Ef.Lang.Knot.Lexicon

import Control.Applicative
import Control.Monad
import Unsafe.Coerce


freshScope
    :: Say Knots lexicon environment Int

freshScope =
    say (FreshScope id)



getScope
    :: Knows Knots lexicon environment
    => Narrative lexicon environment a
    -> environment Int

getScope (Say symbol _) =
    case prj symbol of

        Just x ->
            case x of

                Request i _ _ ->
                    return i

                Respond i _ _ ->
                    return i



linearize
    :: Effect lexicon environment r
    -> Say Knots lexicon environment r

linearize e =
    do
      lexicon <- freshScope
      rewrite lexicon $
          runKnotted e
              (\a' apl -> say (Request lexicon a' apl))
              (\b b'p -> say (Respond lexicon b b'p))



rewrite rewriteLexicon = go
  where

    go (Fail e) =
        Fail e

    go (Return r) =
        Return r

    go (Super m) =
        Super (fmap go m)

    go (Say symbol k) =
        let
          check currentLexicon lexicond =
              if currentLexicon == rewriteLexicon then
                  lexicond
              else
                  ignore

          ignore =
              Say symbol (go . k)

        in
          case prj symbol of

              Just x  ->
                  case x of

                      Request currentLexicon a' _ ->
                          check currentLexicon $
                              closed (unsafeCoerce a')

                      Respond currentLexicon b _ ->
                          check currentLexicon $
                              closed (unsafeCoerce b)

              Nothing -> Say symbol (go . k)



instance Functor m
    => Functor (Knotted fs a' a b' b m)
  where

    fmap f (Knotted w) =
        Knotted $ \up dn -> fmap f (w up dn)



instance Monad m
    => Applicative (Knotted fs a' a b' b m)
  where

    pure a =
        Knotted $ \_ _ -> pure a



    wf <*> wx =
        Knotted $ \up dn -> rewriteAp up dn (runKnotted wf up dn)
      where

        rewriteAp up dn =
            go
          where

            go (Fail err) =
                Fail err

            go (Super m) =
                Super (fmap go m)

            go (Say sym k) =
                Say sym (go . k)

            go (Return f) =
                fmap f (runKnotted wx (unsafeCoerce up)
                                    (unsafeCoerce dn)
                       )



    (*>) = (>>)



instance Monad m
    => Monad (Knotted fs a' a b' b m)
  where

    return =
        pure



    r >>= rs =
        Knotted $ \up dn ->
            do
              v <- runKnotted r (unsafeCoerce up) (unsafeCoerce dn)
              runKnotted (rs v) up dn



instance ( Monad m
         , Monoid r
         ) => Monoid (Knotted fs a' a b' b m r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        Knotted $ \up dn ->
            rewriteMappend up dn (runKnotted w1 up dn)
      where

        rewriteMappend up dn = go
          where

            go (Fail err) =
                Fail err

            go (Super m) =
                Super (fmap go m)

            go (Say sym bp) =
                Say sym (go . bp)

            go (Return result) =
                let
                  routine =
                      runKnotted w2 (unsafeCoerce up) (unsafeCoerce dn)

                in
                  fmap (mappend result) routine



instance MonadPlus m
    => Alternative (Knotted fs a' a b' b m)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus m
    => MonadPlus (Knotted fs a' a b' b m)
  where

    mzero =
        Knotted $ \_ _ -> super mzero

    mplus w0 w1 =
        Knotted $ \up dn ->
            let
              routine =
                  runKnotted w0 (unsafeCoerce up) (unsafeCoerce dn)
            in
              rewriteMplus up dn routine
      where
        rewriteMplus up dn = go
          where

            go (Fail err) =
                Fail err

            go (Return r) =
                Return r

            go (Say sym bp) =
                Say sym (go . bp)

            go (Super m) =
              let
                routine =
                    runKnotted w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                Super (fmap go m `mplus` return routine)



newtype X = X X



closed
    :: X
    -> a

closed (X x) =
    closed x



type Effect fs m r =
    Knotted fs X () () X m r



type Producer b fs m r =
    Knotted fs X () () b m r



producer
    :: forall fs m b r.
       Knows Knots fs m
    => (    (    b
              -> Narrative fs m ()
            )
         -> Narrative fs m r
       )
    -> Producer' b fs m r

producer f =
    Knotted $ \_ dn ->
        do
          let
            lexicondDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond lexicon b =
                say (Respond lexicon b Return)

          i <- lift (getScope lexicondDown)
          f (respond i)



type Consumer a fs m r =
    Knotted fs () a () X m r



consumer
    :: forall fs m a r.
       Knows Knots fs m
    => (    Narrative fs m a
         -> Narrative fs m r
       )
    -> Consumer' a fs m r

consumer f =
    Knotted $ \up _ ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request lexicon =
                say (Request lexicon () Return)

          i <- lift (getScope lexicondUp)
          f (request i)



type Line a b fs m r = Knotted fs () a () b m r



line
    :: forall fs m a b x r.
       Knows Knots fs m
    => (    Narrative fs m a
         -> (    b
              -> Narrative fs m x
            )
         -> Narrative fs m r
       )
    -> Line a b fs m r

line f =
    Knotted $ \up _ ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope lexicondUp)
          let
            request =
                say (Request i () Return)

            respond b =
                say (Respond i b Return)

          f request respond



type Client a' a fs m r =
    Knotted fs a' a () X m r



client
    :: forall fs m a' a r.
       Knows Knots fs m
    => (    (    a'
              -> Narrative fs m a
            )
         -> Narrative fs m r
       )
    -> Client' a' a fs m r

client f =
    Knotted $ \up _ ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope lexicondUp)
          let
            request a =
                say (Request i a Return)

          f request



type Server b' b fs m r =
    Knotted fs X () b' b m r



server
    :: forall fs m b' b r.
       Knows Knots fs m
    => (    (    b
              -> Narrative fs m b'
            )
         -> Narrative fs m r
       )
    -> Server' b' b fs m r

server f =
    Knotted $ \_ dn ->
        do
         let
           lexicondDown =
               dn (unsafeCoerce ()) (unsafeCoerce ())

         i <- lift (getScope lexicondDown)
         let
           respond b' =
               say (Respond i b' Return)

         f respond



newtype Knotted fs a' a b' b m r =
    Knotted
        {
          runKnotted
              :: (forall x.
                      a'
                   -> (    a
                        -> Narrative fs m x
                      )
                   -> Narrative fs m x
                 )
              -> (forall x.
                      b
                   -> (    b'
                        -> Narrative fs m x
                      )
                   -> Narrative fs m x
                 )
             -> Narrative fs m r
        }



knotted
    :: forall fs a a' b b' m r.
       Knows Knots fs m
    => (    (    a
              -> Narrative fs m a'
            )
         -> (    b'
              -> Narrative fs m b
            )
         -> Narrative fs m r
       )
    -> Knotted fs a' a b' b m r

knotted f =
    Knotted $ \up _ ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope lexicondUp)
          let
            request a =
                say (Request i a Return)

            respond b' =
                say (Respond i b' Return)

          f request respond



type Effect' fs m r =
    forall x' x y' y.
    Knotted fs x' x y' y m r



type Producer' b fs m r =
    forall x' x.
    Knotted fs x' x () b m r



type Consumer' a fs m r =
    forall y' y.
    Knotted fs () a y' y m r



type Server' b' b fs m r =
    forall x' x.
    Knotted fs x' x b' b m r



type Client' a' a fs m r =
    forall y' y.
    Knotted fs a' a y' y m r



--------------------------------------------------------------------------------
-- Respond; substitute yields with a function

cat
    :: Knows Knots fs m
    => Line a a fs m r

cat =
    line $ \awt yld ->
        forever (awt >>= yld)



infixl 3 //>
(//>)
    :: forall fs x' x b' b c' c m a'.
       Knows Knots fs m
    => Knotted fs x' x b' b m a'
    -> (    b
         -> Knotted fs x' x c' c m b'
       )
    -> Knotted fs x' x c' c m a'

p0 //> fb =
    Knotted $ \up dn ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope lexicondUp)
          let
            routine =
                runKnotted p0 up (unsafeCoerce dn)

          substituteResponds fb i up dn routine



substituteResponds fb rewriteLexicon up dn =
    go
  where

    go (Fail err) =
        Fail err

    go (Return r) =
        Return r

    go (Super m) =
        Super (fmap go m)

    go (Say sym bp) =
        let
          check currentLexicon lexicond =
              if currentLexicon == rewriteLexicon then
                  lexicond
              else
                  ignore

          ignore =
              Say sym (go . bp)

        in
          case prj sym of

              Just x ->
                  case x of

                      Respond currentLexicon b _ ->
                          check currentLexicon $
                              do
                                let
                                  routine =
                                      runKnotted (fb (unsafeCoerce b))
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
    :: Knows Knots fs m
    => Knotted fs x' x b' b m a'
    -> (    b
         -> Knotted fs x' x c' c m b'
       )
    -> Knotted fs x' x c' c m a'

for =
    (//>)



infixr 3 <\\
(<\\)
    :: Knows Knots fs m
    => (    b
         -> Knotted fs x' x c' c m b'
       )
    -> Knotted fs x' x b' b m a'
    -> Knotted fs x' x c' c m a'

f <\\ p =
    p //> f



infixl 4 \<\
(\<\)
    :: Knows Knots fs m
    => (    b
         -> Knotted fs x' x c' c m b'
       )
    -> (    a
         -> Knotted fs x' x b' b m a'
       )
    -> a
    -> Knotted fs x' x c' c m a'

p1 \<\ p2 = p2 />/ p1



infixr 4 ~>
(~>)
    :: Knows Knots fs m
    => (    a
         -> Knotted fs x' x b' b m a'
       )
    -> (    b
         -> Knotted fs x' x c' c m b'
       )
    -> a
    -> Knotted fs x' x c' c m a'

(~>) =
    (/>/)



infixl 4 <~
(<~)
    :: Knows Knots fs m
    => (    b
         -> Knotted fs x' x c' c m b'
       )
    -> (    a
         -> Knotted fs x' x b' b m a'
       )
    -> a
    -> Knotted fs x' x c' c m a'

g <~ f =
    f ~> g



infixr 4 />/
(/>/)
    :: Knows Knots fs m
    => (    a
         -> Knotted fs x' x b' b m a'
       )
    -> (    b
         -> Knotted fs x' x c' c m b'
       )
    -> a
    -> Knotted fs x' x c' c m a'

(fa />/ fb) a =
    fa a //> fb



--------------------------------------------------------------------------------
-- Request; substitute awaits with a function


infixr 4 >\\
(>\\)
    :: forall fs y' y a' a b' b m c.
       Knows Knots fs m
    => (    b'
         -> Knotted fs a' a y' y m b
       )
    -> Knotted fs b' b y' y m c
    -> Knotted fs a' a y' y m c

fb' >\\ p0 =
    Knotted $ \up dn ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope lexicondUp)
          let
            routine =
                runKnotted p0 (unsafeCoerce up) dn

          substituteRequests fb' i up dn routine

substituteRequests fb' rewriteLexicon up dn p1 = go p1
  where

    go (Fail err) =
        Fail err

    go (Return r) =
        Return r

    go (Super m) =
        Super (fmap go m)

    go (Say sym bp) =
        let
          check currentLexicon lexicond =
              if currentLexicon == rewriteLexicon then
                  lexicond
              else
                  ignore

          ignore =
              Say sym (go . bp)

        in
          case prj sym of

              Just x ->
                  case x of

                      Request currentLexicon b' _ ->
                          check currentLexicon $
                              do
                                let
                                  routine =
                                      runKnotted (fb' (unsafeCoerce b'))
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
    :: Knows Knots fs m
    => (    c'
         -> Knotted fs b' b x' x m c
       )
    -> (    b'
         -> Knotted fs a' a x' x m b
       )
    -> c'
    -> Knotted fs a' a x' x m c

p1 /</ p2 =
    p2 \>\ p1



infixr 5 >~
(>~)
    :: Knows Knots fs m
    => Knotted fs a' a y' y m b
    -> Knotted fs () b y' y m c
    -> Knotted fs a' a y' y m c

p1 >~ p2 =
    (\() -> p1) >\\ p2



infixl 5 ~<
(~<)
    :: Knows Knots fs m
    => Knotted fs () b y' y m c
    -> Knotted fs a' a y' y m b
    -> Knotted fs a' a y' y m c

p2 ~< p1 =
    p1 >~ p2



infixl 5 \>\
(\>\)
    :: Knows Knots fs m
    => (    b'
         -> Knotted fs a' a y' y m b
       )
    -> (    c'
         -> Knotted fs b' b y' y m c
       )
    -> c'
    -> Knotted fs a' a y' y m c

(fb' \>\ fc') c' =
    fb' >\\ fc' c'



infixl 4 \\<
(\\<)
    :: forall fs y' y a' a b' b m c.
       Knows Knots fs m
    => Knotted fs b' b y' y m c
    -> (    b'
         -> Knotted fs a' a y' y m b
       )
    -> Knotted fs a' a y' y m c
p \\< f =
    f >\\ p



--------------------------------------------------------------------------------
-- Push; substitute responds with requests


infixl 7 >>~
(>>~)
    :: forall fs a' a b' b c' c m r. Knows Knots fs m
    => Knotted fs a' a b' b m r
    -> (    b
         -> Knotted fs b' b c' c m r
       )
    -> Knotted fs a' a c' c m r
p0 >>~ fb0 =
    Knotted $ \up dn ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope lexicondUp)
          pushRewrite i up dn fb0 p0



pushRewrite rewriteLexicon up dn fb0 p0 =
    let
      upstream =
          runKnotted p0 (unsafeCoerce up) (unsafeCoerce dn)

      downstream b =
          runKnotted (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)

    in
      goLeft downstream upstream
  where

    goLeft fb = goLeft'
      where

        goLeft' (Return r) =
            Return r

        goLeft' (Fail err) =
            Fail err

        goLeft' (Super m) =
            Super (fmap goLeft' m)

        goLeft' (Say sym bp) =
            let
              check currentLexicon lexicond =
                  if currentLexicon == rewriteLexicon then
                      lexicond
                  else
                      ignore

              ignore =
                  Say sym (goLeft' . bp)

            in
              case prj sym of

                Just x ->
                    case x of

                        Respond currentLexicon b _ ->
                            check currentLexicon $
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

        goRight' (Return r) =
            Return r

        goRight' (Say sym bp) =
            let
              check currentLexicon lexicond =
                  if currentLexicon == rewriteLexicon then
                      lexicond
                  else
                      ignore

              ignore =
                  Say sym (goRight' . bp)
            in
              case prj sym of

                  Just x  ->
                      case x of

                          Request currentLexicon b' _ ->
                              check currentLexicon $
                                  goLeft (unsafeCoerce bp)
                                         (b'p (unsafeCoerce b'))

                          _ ->
                              ignore

                  _ ->
                      ignore




infixl 8 <~<
(<~<)
    :: Knows Knots fs m
    => (    b
         -> Knotted fs b' b c' c m r
       )
    -> (    a
         -> Knotted fs a' a b' b m r
       )
    -> a
    -> Knotted fs a' a c' c m r

p1 <~< p2 =
    p2 >~> p1



infixr 8 >~>
(>~>)
    :: Knows Knots fs m
    => (    _a
          -> Knotted fs a' a b' b m r
       )
    -> (    b
         -> Knotted fs b' b c' c m r
       )
    -> _a
    -> Knotted fs a' a c' c m r

(fa >~> fb) a =
    fa a >>~ fb



infixr 7 ~<<
(~<<)
    :: Knows Knots fs m
    => (    b
         -> Knotted fs b' b c' c m r
       )
    -> Knotted fs a' a b' b m r
    -> Knotted fs a' a c' c m r

k ~<< p =
    p >>~ k



--------------------------------------------------------------------------------
-- Pull; substitute requests with responds


infixr 6 +>>
(+>>) :: forall fs m a' a b' b c' c r. Knows Knots fs m
      => (b' -> Knotted fs a' a b' b m r)
      ->        Knotted fs b' b c' c m r
      ->        Knotted fs a' a c' c m r
fb' +>> p0 =
    Knotted $ \up dn ->
        do
          let
            lexicondUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope lexicondUp)
          pullRewrite i up dn fb' p0

pullRewrite rewriteLexicon up dn fb' p =
    let
      upstream b' =
          runKnotted (fb' b') (unsafeCoerce up) (unsafeCoerce dn)

      downstream =
          runKnotted p (unsafeCoerce up) (unsafeCoerce dn)

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

        goRight' (Return r) =
            Return r

        goRight' (Say sym bp) =
            let
              check currentLexicon lexicond =
                  if currentLexicon == rewriteLexicon then
                      lexicond
                  else
                      ignore

              ignore =
                  Say sym (goRight' . bp)
            in
              case prj sym of

                  Just x ->
                      case x of

                          Request currentLexicon b' _ ->
                              check currentLexicon $
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

        goLeft' (Return r) =
            Return r

        goLeft' (Say sym bp') =
            let
              check currentLexicon lexicond =
                  if currentLexicon == rewriteLexicon then
                      lexicond
                  else
                      ignore

              ignore =
                  Say sym (goLeft' . bp')

            in
              case prj sym of

                  Just x ->
                      case x of

                          Respond currentLexicon b _ ->
                              check currentLexicon $
                                  goRight (unsafeCoerce bp')
                                          (bp (unsafeCoerce b))

                          _ ->
                              ignore

                  _ ->
                      ignore




infixl 7 >->
(>->)
    :: Knows Knots fs m
    => Knotted fs a' a () b m r
    -> Knotted fs () b c' c m r
    -> Knotted fs a' a c' c m r

p1 >-> p2 =
    (\() -> p1) +>> p2


infixr 7 <-<
(<-<)
    :: Knows Knots fs m
    => Knotted fs () b c' c m r
    -> Knotted fs a' a () b m r
    -> Knotted fs a' a c' c m r

p2 <-< p1 =
    p1 >-> p2



infixr 7 <+<
(<+<)
    :: Knows Knots fs m
    => (    c'
         -> Knotted fs b' b c' c m r
       )
    -> (    b'
         -> Knotted fs a' a b' b m r
       )
    -> c'
    -> Knotted fs a' a c' c m r

p1 <+< p2 =
    p2 >+> p1



infixl 7 >+>
(>+>)
    :: Knows Knots fs m
    => (    b'
         -> Knotted fs a' a b' b m r
       )
    -> (    _c'
         -> Knotted fs b' b c' c m r
       )
    -> _c'
    -> Knotted fs a' a c' c m r

(fb' >+> fc') c' =
    fb' +>> fc' c'



infixl 6 <<+
(<<+)
    :: forall fs m a' a b' b c' c r. Knows Knots fs m
    => Knotted fs b' b c' c m r
    -> (    b'
         -> Knotted fs a' a b' b m r
       )
    -> Knotted fs a' a c' c m r

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



{-# INLINE linearize #-}
{-# INLINE closed #-}
{-# INLINE producer #-}
{-# INLINE consumer #-}
{-# INLINE line #-}
{-# INLINE client #-}
{-# INLINE server #-}
{-# INLINE knotted #-}

{-# INLINE freshScope #-}
{-# INLINE getScope #-}
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
