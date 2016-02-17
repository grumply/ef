{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Knot.Messages.Single
    ( SingleKnot(..)
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

    , SingleKnotted(..)
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



import Ef.Narrative

import Control.Applicative
import Control.Monad
import Unsafe.Coerce

import Ef.Messages


data SingleKnot k where
    Request :: a' -> (a -> Narrative self super r) -> SingleKnot k
    Respond :: b -> (b' -> Narrative self super r) -> SingleKnot k


linearize :: ('[SingleKnot] <: self, Monad super)
          => Effect self super r -> Invoke SingleKnot self super r
linearize e = do
    rewrite $
        runSingleKnotted e
            (\a' apl -> self (Request a' apl))
            (\b b'p -> self (Respond b b'p))


rewrite :: forall self super result.
           ('[SingleKnot] <: self, Monad super)
        => Narrative self super result -> Narrative self super result
rewrite = transform go
  where

    go :: forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result
    go message k =
        let ignore = Say message (transform go . k)
        in case prj message of
               Just (Request a' _) -> closed (unsafeCoerce a')
               Just (Respond b _) -> closed (unsafeCoerce b)
               Nothing -> Say message (transform go . k)


instance Functor super
    => Functor (SingleKnotted a' a b' b self super)
  where

    fmap f (SingleKnotted w) =
        SingleKnotted $ \up dn -> fmap f (w up dn)


instance Monad super
    => Applicative (SingleKnotted a' a b' b self super)
  where

    pure a =
        SingleKnotted $ \_ _ -> pure a

    wf <*> wx =
        SingleKnotted $ \up dn -> do
            f <- runSingleKnotted wf up dn
            fmap f (runSingleKnotted wx (unsafeCoerce up) (unsafeCoerce dn))

    (*>) = (>>)


instance Monad super
    => Monad (SingleKnotted a' a b' b self super)
  where

    return = pure

    r >>= rs =
        SingleKnotted $ \up dn -> do
            v <- runSingleKnotted r (unsafeCoerce up) (unsafeCoerce dn)
            runSingleKnotted (rs v) up dn


instance ( Monad super
         , Monoid r
         ) => Monoid (SingleKnotted a' a b' b self super r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        SingleKnotted $ \up dn -> do
            result <- runSingleKnotted w1 up dn
            fmap (mappend result) $ runSingleKnotted w2 (unsafeCoerce up) (unsafeCoerce dn)



instance MonadPlus super
    => Alternative (SingleKnotted a' a b' b self super)
  where

    empty = mzero

    (<|>) = mplus


-- what does this look like without inspecting Super since that was the entire
-- point of implementing 'transform'?
instance MonadPlus super
    => MonadPlus (SingleKnotted a' a b' b self super)
  where

    mzero =
        SingleKnotted $ \_ _ -> super mzero

    mplus w0 w1 =
        SingleKnotted $ \up dn ->
            let
              routine =
                  runSingleKnotted w0 (unsafeCoerce up) (unsafeCoerce dn)
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
                    runSingleKnotted w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                Super (fmap go sup `mplus` return routine)


newtype X = X X


closed :: X -> a
closed (X x) = closed x


type Effect self super r = SingleKnotted X () () X self super r

type Producer b self super r = SingleKnotted X () () b self super r


producer
    :: forall self super b r.
       ('[SingleKnot] <: self, Monad super)
    => ((b -> Narrative self super ()) -> Narrative self super r)
    -> Producer' b self super r

producer f =
    SingleKnotted $ \_ dn ->
        do
          let
            scopedDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond :: b -> Narrative self super ()
            respond b =
                self (Respond b Return)

          f respond


type Consumer a self super r = SingleKnotted () a () X self super r


consumer
    :: forall self super a r.
       ('[SingleKnot] <: self, Monad super)
    => (Narrative self super a -> Narrative self super r)
    -> Consumer' a self super r

consumer f =
    SingleKnotted $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request :: Narrative self super a
            request =
                self (Request () Return)

          f request


type Line a b self super r = SingleKnotted () a () b self super r


line
    :: forall self super a b r.
       ('[SingleKnot] <: self, Monad super)
    => (Narrative self super a -> (b -> Narrative self super ()) -> Narrative self super r)
    -> Line a b self super r

line f =
    SingleKnotted $ \up _ ->
        do
          let
            request =
                self (Request () Return)

            respond b =
                self (Respond b Return)

          f request respond


type Client a' a self super r = SingleKnotted a' a () X self super r


type Server b' b self super r = SingleKnotted X () b' b self super r


newtype SingleKnotted a' a b' b self super r =
    SingleKnotted
        {
          runSingleKnotted
              :: (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
              -> (forall x. b -> (b' -> Narrative self super x) -> Narrative self super x)
              -> Narrative self super r
        }



knotted
    :: forall self a a' b b' super r.
       ('[SingleKnot] <: self, Monad super)
    => ((a' -> Narrative self super a) -> (b -> Narrative self super b') -> Narrative self super r)
    -> SingleKnotted a' a b' b self super r

knotted f =
    SingleKnotted $ \up _ ->
        do
            let request a = self (Request a Return)
                respond b' = self (Respond b' Return)
            f request respond

type Effect' self super r = forall x' x y' y. SingleKnotted x' x y' y self super r

type Producer' b self super r = forall x' x. SingleKnotted x' x () b self super r

type Consumer' a self super r = forall y' y. SingleKnotted () a y' y self super r

type Server' b' b self super r = forall x' x. SingleKnotted x' x b' b self super r

type Client' a' a self super r = forall y' y. SingleKnotted a' a y' y self super r

--------------------------------------------------------------------------------
-- Respond; substitute yields

cat :: ('[SingleKnot] <: self, Monad super) => Line a a self super r
cat = line $ \awt yld -> forever (awt >>= yld)


infixl 3 //>
(//>) :: ('[SingleKnot] <: self, Monad super)
      => SingleKnotted x' x b' b self super a'
      -> (b -> SingleKnotted x' x c' c self super b')
      -> SingleKnotted x' x c' c self super a'
p0 //> fb =
    SingleKnotted $ \up dn -> do
        let routine = runSingleKnotted p0 up (unsafeCoerce dn)
        substituteResponds fb up dn routine


substituteResponds
    :: forall self super x' x c' c b' b a'.
       ('[SingleKnot] <: self, Monad super)
    => (b -> SingleKnotted x' x c' c self super b')
    -> (forall r. x' -> (x -> Narrative self super r) -> Narrative self super r)
    -> (forall r. c -> (c' -> Narrative self super r) -> Narrative self super r)
    -> Narrative self super a'
    -> Narrative self super a'
substituteResponds fb up dn =
    transform go
  where

    go :: forall z. Messages self z -> (z -> Narrative self super a') -> Narrative self super a'
    go message k =
        let ignore = Say message (transform go . k)
        in case prj message of

               Just x ->
                   case x of

                       Respond b _ -> do
                               let routine = runSingleKnotted (fb (unsafeCoerce b))
                                                        (unsafeCoerce up)
                                                        (unsafeCoerce dn)
                               res <- routine
                               let continue = k (unsafeCoerce res)
                               transform go continue
                       _ -> ignore
               _ -> ignore


for :: ('[SingleKnot] <: self, Monad super)
    => SingleKnotted x' x b' b self super a'
    -> (b -> SingleKnotted x' x c' c self super b')
    -> SingleKnotted x' x c' c self super a'
for = (//>)


infixr 3 <\\
(<\\) :: ('[SingleKnot] <: self, Monad super)
      => (b -> SingleKnotted x' x c' c self super b')
      -> SingleKnotted x' x b' b self super a'
      -> SingleKnotted x' x c' c self super a'
f <\\ p = p //> f


infixl 4 \<\
(\<\) :: ('[SingleKnot] <: self, Monad super)
      => (b -> SingleKnotted x' x c' c self super b')
      -> (a -> SingleKnotted x' x b' b self super a')
      -> a
      -> SingleKnotted x' x c' c self super a'
p1 \<\ p2 = p2 />/ p1


infixr 4 ~>
(~>) :: ('[SingleKnot] <: self, Monad super)
     => (a -> SingleKnotted x' x b' b self super a')
     -> (b -> SingleKnotted x' x c' c self super b')
     -> a
     -> SingleKnotted x' x c' c self super a'
(~>) = (/>/)


infixl 4 <~
(<~) :: ('[SingleKnot] <: self, Monad super)
     => (b -> SingleKnotted x' x c' c self super b')
     -> (a -> SingleKnotted x' x b' b self super a')
     -> a
     -> SingleKnotted x' x c' c self super a'
g <~ f = f ~> g


infixr 4 />/
(/>/) :: ('[SingleKnot] <: self, Monad super)
      => (a -> SingleKnotted x' x b' b self super a')
      -> (b -> SingleKnotted x' x c' c self super b')
      -> a
      -> SingleKnotted x' x c' c self super a'
(fa />/ fb) a = fa a //> fb


--------------------------------------------------------------------------------
-- Request; substitute awaits


infixr 4 >\\
(>\\) :: ('[SingleKnot] <: self, Monad super)
      => (b' -> SingleKnotted a' a y' y self super b)
      -> SingleKnotted b' b y' y self super c
      -> SingleKnotted a' a y' y self super c
fb' >\\ p0 =
    SingleKnotted $ \up dn -> do
        let routine = runSingleKnotted p0 (unsafeCoerce up) dn
        substituteRequests fb' up dn routine


substituteRequests
    :: forall self super x' x c' c b' b a'.
       ('[SingleKnot] <: self, Monad super)
    => (b -> SingleKnotted x' x c' c self super b')
    -> (forall r. x' -> (x -> Narrative self super r) -> Narrative self super r)
    -> (forall r. c -> (c' -> Narrative self super r) -> Narrative self super r)
    -> Narrative self super a'
    -> Narrative self super a'
substituteRequests fb' up dn =
    transform go
  where

    go :: forall z. Messages self z -> (z -> Narrative self super a') -> Narrative self super a'
    go message k =
        let
          ignore =
              Say message (transform go . k)

        in
          case prj message of

              Just x ->
                  case x of

                      Request b' _ -> do
                              let routine = runSingleKnotted (fb' (unsafeCoerce b'))
                                                       (unsafeCoerce up)
                                                       (unsafeCoerce dn)
                              res <- routine
                              let continue = k (unsafeCoerce res)
                              transform go continue
                      _ -> ignore
              _ -> ignore


infixr 5 /</
(/</) :: ('[SingleKnot] <: self, Monad super)
      => (c' -> SingleKnotted b' b x' x self super c)
      -> (b' -> SingleKnotted a' a x' x self super b)
      -> c'
      -> SingleKnotted a' a x' x self super c
p1 /</ p2 = p2 \>\ p1


infixr 5 >~
(>~) :: ('[SingleKnot] <: self, Monad super)
     => SingleKnotted a' a y' y self super b
     -> SingleKnotted () b y' y self super c
     -> SingleKnotted a' a y' y self super c
p1 >~ p2 = (\() -> p1) >\\ p2


infixl 5 ~<
(~<) :: ('[SingleKnot] <: self, Monad super)
     => SingleKnotted () b y' y self super c
     -> SingleKnotted a' a y' y self super b
     -> SingleKnotted a' a y' y self super c
p2 ~< p1 = p1 >~ p2


infixl 5 \>\
(\>\) :: ('[SingleKnot] <: self, Monad super)
      => (b' -> SingleKnotted a' a y' y self super b)
      -> (c' -> SingleKnotted b' b y' y self super c)
      -> c'
      -> SingleKnotted a' a y' y self super c
(fb' \>\ fc') c' = fb' >\\ fc' c'


infixl 4 \\<
(\\<) :: ('[SingleKnot] <: self, Monad super)
      => SingleKnotted b' b y' y self super c
      -> (b' -> SingleKnotted a' a y' y self super b)
      -> SingleKnotted a' a y' y self super c
p \\< f = f >\\ p


--------------------------------------------------------------------------------
-- Push; substitute responds with requests


infixl 7 >>~
(>>~)
    :: forall self a' a b' b c' c super r.
       Knows SingleKnot self super
    => SingleKnotted a' a b' b self super r
    -> (b -> SingleKnotted b' b c' c self super r)
    -> SingleKnotted a' a c' c self super r
p0 >>~ fb0 =
    SingleKnotted $ \up dn -> do
        pushRewrite up dn fb0 p0


pushRewrite
    :: forall self super r a' a b' b c' c.
       ('[SingleKnot] <: self, Monad super)
    => (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b -> SingleKnotted b' b c' c self super r)
    -> SingleKnotted a' a b' b self super r
    -> Narrative self super r
pushRewrite up dn fb0 p0 =
    let upstream = runSingleKnotted p0 (unsafeCoerce up) (unsafeCoerce dn)
        downstream b = runSingleKnotted (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)
    in goLeft downstream upstream
  where

    goLeft fb =
        transform goLeft'
      where

        goLeft' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goLeft' message k =
            let ignore = Say message (transform goLeft' . k)
            in case prj message of
                   Just x ->
                       case x of
                          Respond b _ ->
                            goRight (unsafeCoerce k) (fb (unsafeCoerce b))
                          _ -> ignore
                   _ -> ignore

    goRight b'p =
        transform goRight'
      where

        goRight' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goRight' message k =
            let ignore = Say message (transform goRight' . k)
            in case prj message of
                   Just x  ->
                       case x of
                           Request b' _ ->
                              goLeft (unsafeCoerce k) (b'p (unsafeCoerce b'))
                           _ -> ignore
                   _ -> ignore


infixl 8 <~<
(<~<) :: ('[SingleKnot] <: self, Monad super)
      => (b -> SingleKnotted b' b c' c self super r)
      -> (a -> SingleKnotted a' a b' b self super r)
      -> a
      -> SingleKnotted a' a c' c self super r
p1 <~< p2 = p2 >~> p1


infixr 8 >~>
(>~>) :: ('[SingleKnot] <: self, Monad super)
      => (_a -> SingleKnotted a' a b' b self super r)
      -> (b -> SingleKnotted b' b c' c self super r)
      -> _a
      -> SingleKnotted a' a c' c self super r
(fa >~> fb) a = fa a >>~ fb


infixr 7 ~<<
(~<<) :: ('[SingleKnot] <: self, Monad super)
      => (b -> SingleKnotted b' b c' c self super r)
      -> SingleKnotted a' a b' b self super r
      -> SingleKnotted a' a c' c self super r
k ~<< p = p >>~ k


--------------------------------------------------------------------------------
-- Pull; substitute requests with responds


infixr 6 +>>
(+>>) :: ('[SingleKnot] <: self, Monad super)
      => (b' -> SingleKnotted a' a b' b self super r)
      ->        SingleKnotted b' b c' c self super r
      ->        SingleKnotted a' a c' c self super r
fb' +>> p0 =
    SingleKnotted $ \up dn -> do
        pullRewrite up dn fb' p0


pullRewrite
    :: forall self super a' a b' b c' c r.
       ('[SingleKnot] <: self, Monad super)
    => (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b' -> SingleKnotted a' a b' b self super r)
    -> SingleKnotted b' b c' c self super r
    -> Narrative self super r
pullRewrite up dn fb' p =
    let upstream b' = runSingleKnotted (fb' b') (unsafeCoerce up) (unsafeCoerce dn)
        downstream = runSingleKnotted p (unsafeCoerce up) (unsafeCoerce dn)
    in goRight upstream downstream
  where

    goRight fb'' =
        transform goRight'
      where

        goRight' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goRight' message k =
            let ignore = Say message (transform goRight' . k)
            in case prj message of
                   Just x ->
                       case x of
                           Request b' _ ->
                               goLeft (unsafeCoerce k) (fb'' (unsafeCoerce b'))
                           _ -> ignore
                   _ -> ignore

    goLeft bp =
        transform goLeft'
      where

        goLeft' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goLeft' message k' =
            let ignore = Say message (transform goLeft' . k')
            in case prj message of
                   Just x ->
                       case x of
                           Respond b _ ->
                               goRight (unsafeCoerce k') (bp (unsafeCoerce b))
                           _ -> ignore
                   _ -> ignore


infixl 7 >->
(>->) :: ('[SingleKnot] <: self, Monad super)
      => SingleKnotted a' a () b self super r
      -> SingleKnotted () b c' c self super r
      -> SingleKnotted a' a c' c self super r
p1 >-> p2 = (\() -> p1) +>> p2


infixr 7 <-<
(<-<) :: ('[SingleKnot] <: self, Monad super)
      => SingleKnotted () b c' c self super r
      -> SingleKnotted a' a () b self super r
      -> SingleKnotted a' a c' c self super r
p2 <-< p1 = p1 >-> p2


infixr 7 <+<
(<+<) :: ('[SingleKnot] <: self, Monad super)
      => (c' -> SingleKnotted b' b c' c self super r)
      -> (b' -> SingleKnotted a' a b' b self super r)
      -> c'
      -> SingleKnotted a' a c' c self super r
p1 <+< p2 = p2 >+> p1


infixl 7 >+>
(>+>) :: ('[SingleKnot] <: self, Monad super)
      => (b' -> SingleKnotted a' a b' b self super r)
      -> (_c' -> SingleKnotted b' b c' c self super r)
      -> _c'
      -> SingleKnotted a' a c' c self super r
(fb' >+> fc') c' = fb' +>> fc' c'


infixl 6 <<+
(<<+) :: ('[SingleKnot] <: self, Monad super)
      => SingleKnotted b' b c' c self super r
      -> (b' -> SingleKnotted a' a b' b self super r)
      -> SingleKnotted a' a c' c self super r
p <<+ fb = fb +>> p


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
