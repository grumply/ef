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
    -- , client

    , Server
    , Server'
    -- , server

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



instance Functor environment
    => Functor (Knotted a' a b' b lexicon environment)
  where

    fmap f (Knotted w) =
        Knotted $ \up dn -> fmap f (w up dn)



instance Monad environment
    => Applicative (Knotted a' a b' b lexicon environment)
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



instance Monad environment
    => Monad (Knotted a' a b' b lexicon environment)
  where

    return =
        pure



    r >>= rs =
        Knotted $ \up dn ->
            do
              v <- runKnotted r (unsafeCoerce up) (unsafeCoerce dn)
              runKnotted (rs v) up dn



instance ( Monad environment
         , Monoid r
         ) => Monoid (Knotted a' a b' b lexicon environment r)
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



instance MonadPlus environment
    => Alternative (Knotted a' a b' b lexicon environment)
  where

    empty =
        mzero



    (<|>) =
        mplus



instance MonadPlus environment
    => MonadPlus (Knotted a' a b' b lexicon environment)
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

            go (Super sup) =
              let
                routine =
                    runKnotted w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                Super (fmap go sup `mplus` return routine)



newtype X = X X



closed
    :: X -> a

closed (X x) =
    closed x



type Effect lexicon environment r =
    Knotted X () () X lexicon environment r



type Producer b lexicon environment r =
    Knotted X () () b lexicon environment r



producer
    :: forall lexicon environment b r.
       Knows Knots lexicon environment
    => ((b -> Narrative lexicon environment ()) -> Narrative lexicon environment r)
    -> Producer' b lexicon environment r

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



type Consumer a lexicon environment r =
    Knotted () a () X lexicon environment r



consumer
    :: forall lexicon environment a r.
       Knows Knots lexicon environment
    => (Narrative lexicon environment a -> Narrative lexicon environment r)
    -> Consumer' a lexicon environment r

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



type Line a b lexicon environment r = Knotted () a () b lexicon environment r



line
    :: forall lexicon environment a b r.
       Knows Knots lexicon environment
    => (Narrative lexicon environment a -> (b -> Narrative lexicon environment ()) -> Narrative lexicon environment r)
    -> Line a b lexicon environment r

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



type Client a' a lexicon environment r =
    Knotted a' a () X lexicon environment r



type Server b' b lexicon environment r =
    Knotted X () b' b lexicon environment r



newtype Knotted a' a b' b lexicon environment r =
    Knotted
        {
          runKnotted
              :: (forall x. a' -> (a -> Narrative lexicon environment x) -> Narrative lexicon environment x)
              -> (forall x. b -> (b' -> Narrative lexicon environment x) -> Narrative lexicon environment x)
             -> Narrative lexicon environment r
        }



knotted
    :: forall lexicon a a' b b' environment r.
       Knows Knots lexicon environment
    => ((a' -> Narrative lexicon environment a) -> (b -> Narrative lexicon environment b') -> Narrative lexicon environment r)
    -> Knotted a' a b' b lexicon environment r

knotted f =
    Knotted $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            request a =
                say (Request i a Return)

            respond b' =
                say (Respond i b' Return)

          f request respond



type Effect' lexicon environment r =
    forall x' x y' y.
    Knotted x' x y' y lexicon environment r



type Producer' b lexicon environment r =
    forall x' x.
    Knotted x' x () b lexicon environment r



type Consumer' a lexicon environment r =
    forall y' y.
    Knotted () a y' y lexicon environment r



type Server' b' b lexicon environment r =
    forall x' x.
    Knotted x' x b' b lexicon environment r



type Client' a' a lexicon environment r =
    forall y' y.
    Knotted a' a y' y lexicon environment r



--------------------------------------------------------------------------------
-- Respond; substitute yields

cat
    :: Knows Knots lexicon environment
    => Line a a lexicon environment r

cat =
    line $ \awt yld ->
        forever (awt >>= yld)



infixl 3 //>
(//>)
    :: forall lexicon x' x b' b c' c environment a'.
       Knows Knots lexicon environment
    => Knotted x' x b' b lexicon environment a'
    -> (b -> Knotted x' x c' c lexicon environment b')
    -> Knotted x' x c' c lexicon environment a'

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
    :: Knows Knots lexicon environment
    => Knotted x' x b' b lexicon environment a'
    -> (b -> Knotted x' x c' c lexicon environment b')
    -> Knotted x' x c' c lexicon environment a'

for =
    (//>)



infixr 3 <\\
(<\\)
    :: Knows Knots lexicon environment
    => (b -> Knotted x' x c' c lexicon environment b')
    -> Knotted x' x b' b lexicon environment a'
    -> Knotted x' x c' c lexicon environment a'

f <\\ p =
    p //> f



infixl 4 \<\
(\<\)
    :: Knows Knots lexicon environment
    => (b -> Knotted x' x c' c lexicon environment b')
    -> (a -> Knotted x' x b' b lexicon environment a')
    -> a
    -> Knotted x' x c' c lexicon environment a'

p1 \<\ p2 = p2 />/ p1



infixr 4 ~>
(~>)
    :: Knows Knots lexicon environment
    => (a -> Knotted x' x b' b lexicon environment a')
    -> (b -> Knotted x' x c' c lexicon environment b')
    -> a
    -> Knotted x' x c' c lexicon environment a'

(~>) =
    (/>/)



infixl 4 <~
(<~)
    :: Knows Knots lexicon environment
    => (b -> Knotted x' x c' c lexicon environment b')
    -> (a -> Knotted x' x b' b lexicon environment a')
    -> a
    -> Knotted x' x c' c lexicon environment a'

g <~ f =
    f ~> g



infixr 4 />/
(/>/)
    :: Knows Knots lexicon environment
    => (a -> Knotted x' x b' b lexicon environment a')
    -> (b -> Knotted x' x c' c lexicon environment b')
    -> a
    -> Knotted x' x c' c lexicon environment a'

(fa />/ fb) a =
    fa a //> fb



--------------------------------------------------------------------------------
-- Request; substitute awaits


infixr 4 >\\
(>\\)
    :: forall lexicon y' y a' a b' b environment c.
       Knows Knots lexicon environment
    => (b' -> Knotted a' a y' y lexicon environment b)
    -> Knotted b' b y' y lexicon environment c
    -> Knotted a' a y' y lexicon environment c

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
    :: Knows Knots lexicon environment
    => (c' -> Knotted b' b x' x lexicon environment c)
    -> (b' -> Knotted a' a x' x lexicon environment b)
    -> c'
    -> Knotted a' a x' x lexicon environment c

p1 /</ p2 =
    p2 \>\ p1



infixr 5 >~
(>~)
    :: Knows Knots lexicon environment
    => Knotted a' a y' y lexicon environment b
    -> Knotted () b y' y lexicon environment c
    -> Knotted a' a y' y lexicon environment c

p1 >~ p2 =
    (\() -> p1) >\\ p2



infixl 5 ~<
(~<)
    :: Knows Knots lexicon environment
    => Knotted () b y' y lexicon environment c
    -> Knotted a' a y' y lexicon environment b
    -> Knotted a' a y' y lexicon environment c

p2 ~< p1 =
    p1 >~ p2



infixl 5 \>\
(\>\)
    :: Knows Knots lexicon environment
    => (b' -> Knotted a' a y' y lexicon environment b)
    -> (c' -> Knotted b' b y' y lexicon environment c)
    -> c'
    -> Knotted a' a y' y lexicon environment c

(fb' \>\ fc') c' =
    fb' >\\ fc' c'



infixl 4 \\<
(\\<)
    :: forall lexicon y' y a' a b' b environment c.
       Knows Knots lexicon environment
    => Knotted b' b y' y lexicon environment c
    -> (b' -> Knotted a' a y' y lexicon environment b)
    -> Knotted a' a y' y lexicon environment c
p \\< f =
    f >\\ p



--------------------------------------------------------------------------------
-- Push; substitute responds with requests


infixl 7 >>~
(>>~)
    :: forall lexicon a' a b' b c' c environment r. Knows Knots lexicon environment
    => Knotted a' a b' b lexicon environment r
    -> (b -> Knotted b' b c' c lexicon environment r)
    -> Knotted a' a c' c lexicon environment r
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
    :: Knows Knots lexicon environment
    => (b -> Knotted b' b c' c lexicon environment r)
    -> (a -> Knotted a' a b' b lexicon environment r)
    -> a
    -> Knotted a' a c' c lexicon environment r

p1 <~< p2 =
    p2 >~> p1



infixr 8 >~>
(>~>)
    :: Knows Knots lexicon environment
    => (_a -> Knotted a' a b' b lexicon environment r)
    -> (b -> Knotted b' b c' c lexicon environment r)
    -> _a
    -> Knotted a' a c' c lexicon environment r

(fa >~> fb) a =
    fa a >>~ fb



infixr 7 ~<<
(~<<)
    :: Knows Knots lexicon environment
    => (b -> Knotted b' b c' c lexicon environment r)
    -> Knotted a' a b' b lexicon environment r
    -> Knotted a' a c' c lexicon environment r

k ~<< p =
    p >>~ k



--------------------------------------------------------------------------------
-- Pull; substitute requests with responds


infixr 6 +>>
(+>>) :: forall lexicon environment a' a b' b c' c r. Knows Knots lexicon environment
      => (b' -> Knotted a' a b' b lexicon environment r)
      ->        Knotted b' b c' c lexicon environment r
      ->        Knotted a' a c' c lexicon environment r
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
    :: Knows Knots lexicon environment
    => Knotted a' a () b lexicon environment r
    -> Knotted () b c' c lexicon environment r
    -> Knotted a' a c' c lexicon environment r

p1 >-> p2 =
    (\() -> p1) +>> p2


infixr 7 <-<
(<-<)
    :: Knows Knots lexicon environment
    => Knotted () b c' c lexicon environment r
    -> Knotted a' a () b lexicon environment r
    -> Knotted a' a c' c lexicon environment r

p2 <-< p1 =
    p1 >-> p2



infixr 7 <+<
(<+<)
    :: Knows Knots lexicon environment
    => (c' -> Knotted b' b c' c lexicon environment r)
    -> (b' -> Knotted a' a b' b lexicon environment r)
    -> c'
    -> Knotted a' a c' c lexicon environment r

p1 <+< p2 =
    p2 >+> p1



infixl 7 >+>
(>+>)
    :: Knows Knots lexicon environment
    => (b' -> Knotted a' a b' b lexicon environment r)
    -> (_c' -> Knotted b' b c' c lexicon environment r)
    -> _c'
    -> Knotted a' a c' c lexicon environment r

(fb' >+> fc') c' =
    fb' +>> fc' c'



infixl 6 <<+
(<<+)
    :: forall lexicon environment a' a b' b c' c r. Knows Knots lexicon environment
    => Knotted b' b c' c lexicon environment r
    -> (b' -> Knotted a' a b' b lexicon environment r)
    -> Knotted a' a c' c lexicon environment r

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
-- {-# INLINE client #-}
-- {-# INLINE server #-}
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
