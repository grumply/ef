{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing -fno-warn-missing-methods #-}
module Ef.Bidir
    ( Bidir(..)
    , bidir
    , runBidir

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

    , Server
    , Server'

    , Bi(..)
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

-- This module is not for general use!
-- This implementation must be /very/ carefully used.
-- Use only when needing a unique global eventing system.

import Ef

import Control.Applicative
import Control.Monad
import Unsafe.Coerce

data Bidir k where
    Bidir :: k -> Bidir k
    Request :: a' -> (a -> Narrative self super r) -> Bidir k
    Respond :: b -> (b' -> Narrative self super r) -> Bidir k

instance Ma Bidir Bidir

bidir :: (Monad super, '[Bidir] <. traits)
      => Trait Bidir traits super
bidir = Bidir return
{-# INLINE bidir #-}

runBidir :: ('[Bidir] <: self, Monad super)
         => Effect self super r -> Narrative self super r
runBidir e = do
    rewrite $
        runBi e
            (\a' apl -> self (Request a' apl))
            (\b b'p -> self (Respond b b'p))

rewrite :: forall self super result.
           ('[Bidir] <: self, Monad super)
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
    => Functor (Bi a' a b' b self super)
  where

    fmap f (Bi w) =
        Bi $ \up dn -> fmap f (w up dn)

instance Monad super
    => Applicative (Bi a' a b' b self super)
  where

    pure a =
        Bi $ \_ _ -> pure a

    wf <*> wx =
        Bi $ \up dn -> do
            f <- runBi wf up dn
            fmap f (runBi wx (unsafeCoerce up) (unsafeCoerce dn))

    (*>) = (>>)

instance Monad super
    => Monad (Bi a' a b' b self super)
  where

    return = pure

    r >>= rs =
        Bi $ \up dn -> do
            v <- runBi r (unsafeCoerce up) (unsafeCoerce dn)
            runBi (rs v) up dn

instance ( Monad super
         , Monoid r
         ) => Monoid (Bi a' a b' b self super r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        Bi $ \up dn -> do
            result <- runBi w1 up dn
            fmap (mappend result) $ runBi w2 (unsafeCoerce up) (unsafeCoerce dn)

instance MonadPlus super
    => Alternative (Bi a' a b' b self super)
  where

    empty = mzero

    (<|>) = mplus

-- what does this look like without inspecting Super since that was the entire
-- point of implementing 'transform'?
instance MonadPlus super
    => MonadPlus (Bi a' a b' b self super)
  where

    mzero =
        Bi $ \_ _ -> super mzero

    mplus w0 w1 =
        Bi $ \up dn ->
            let
              routine =
                  runBi w0 (unsafeCoerce up) (unsafeCoerce dn)
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
                    runBi w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                Super (fmap go sup `mplus` return routine)

newtype X = X X

closed :: X -> a
closed (X x) = closed x

type Effect self super r = Bi X () () X self super r

type Producer b self super r = Bi X () () b self super r

producer :: forall self super b r. ('[Bidir] <: self, Monad super)
         => ((b -> Narrative self super ()) -> Narrative self super r)
         -> Producer' b self super r
producer f =
    Bi $ \_ dn ->
        do
          let
            scopedDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond :: b -> Narrative self super ()
            respond b =
                self (Respond b Return)

          f respond

type Consumer a self super r = Bi () a () X self super r

consumer :: forall self super a r. ('[Bidir] <: self, Monad super)
         => (Narrative self super a -> Narrative self super r)
         -> Consumer' a self super r
consumer f =
    Bi $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request :: Narrative self super a
            request =
                self (Request () Return)

          f request

type Line a b self super r = Bi () a () b self super r

line :: forall self super a b r. ('[Bidir] <: self, Monad super)
     => (Narrative self super a -> (b -> Narrative self super ()) -> Narrative self super r)
     -> Line a b self super r
line f =
    Bi $ \up _ ->
        do
          let
            request =
                self (Request () Return)

            respond b =
                self (Respond b Return)

          f request respond

type Client a' a self super r = Bi a' a () X self super r

type Server b' b self super r = Bi X () b' b self super r

newtype Bi a' a b' b self super r =
    Bi
        {
          runBi
              :: (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
              -> (forall x. b -> (b' -> Narrative self super x) -> Narrative self super x)
              -> Narrative self super r
        }

knotted :: forall self a a' b b' super r. ('[Bidir] <: self, Monad super)
        => ((a' -> Narrative self super a) -> (b -> Narrative self super b') -> Narrative self super r)
        -> Bi a' a b' b self super r
knotted f =
    Bi $ \up _ ->
        do
            let request a = self (Request a Return)
                respond b' = self (Respond b' Return)
            f request respond

type Effect' self super r = forall x' x y' y. Bi x' x y' y self super r

type Producer' b self super r = forall x' x. Bi x' x () b self super r

type Consumer' a self super r = forall y' y. Bi () a y' y self super r

type Server' b' b self super r = forall x' x. Bi x' x b' b self super r

type Client' a' a self super r = forall y' y. Bi a' a y' y self super r

--------------------------------------------------------------------------------
-- Respond; substitute yields

cat :: ('[Bidir] <: self, Monad super) => Line a a self super r
cat = line $ \awt yld -> forever (awt >>= yld)

infixl 3 //>
(//>) :: ('[Bidir] <: self, Monad super)
      => Bi x' x b' b self super a'
      -> (b -> Bi x' x c' c self super b')
      -> Bi x' x c' c self super a'
p0 //> fb =
    Bi $ \up dn -> do
        let routine = runBi p0 up (unsafeCoerce dn)
        substituteResponds fb up dn routine

substituteResponds
    :: forall self super x' x c' c b' b a'.
       ('[Bidir] <: self, Monad super)
    => (b -> Bi x' x c' c self super b')
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
                               let routine = runBi (fb (unsafeCoerce b))
                                                        (unsafeCoerce up)
                                                        (unsafeCoerce dn)
                               res <- routine
                               let continue = k (unsafeCoerce res)
                               transform go continue
                       _ -> ignore
               _ -> ignore

for :: ('[Bidir] <: self, Monad super)
    => Bi x' x b' b self super a'
    -> (b -> Bi x' x c' c self super b')
    -> Bi x' x c' c self super a'
for = (//>)

infixr 3 <\\
(<\\) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi x' x c' c self super b')
      -> Bi x' x b' b self super a'
      -> Bi x' x c' c self super a'
f <\\ p = p //> f

infixl 4 \<\
(\<\) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi x' x c' c self super b')
      -> (a -> Bi x' x b' b self super a')
      -> a
      -> Bi x' x c' c self super a'
p1 \<\ p2 = p2 />/ p1

infixr 4 ~>
(~>) :: ('[Bidir] <: self, Monad super)
     => (a -> Bi x' x b' b self super a')
     -> (b -> Bi x' x c' c self super b')
     -> a
     -> Bi x' x c' c self super a'
(~>) = (/>/)

infixl 4 <~
(<~) :: ('[Bidir] <: self, Monad super)
     => (b -> Bi x' x c' c self super b')
     -> (a -> Bi x' x b' b self super a')
     -> a
     -> Bi x' x c' c self super a'
g <~ f = f ~> g

infixr 4 />/
(/>/) :: ('[Bidir] <: self, Monad super)
      => (a -> Bi x' x b' b self super a')
      -> (b -> Bi x' x c' c self super b')
      -> a
      -> Bi x' x c' c self super a'
(fa />/ fb) a = fa a //> fb

--------------------------------------------------------------------------------
-- Request; substitute awaits

infixr 4 >\\
(>\\) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a y' y self super b)
      -> Bi b' b y' y self super c
      -> Bi a' a y' y self super c
fb' >\\ p0 =
    Bi $ \up dn -> do
        let routine = runBi p0 (unsafeCoerce up) dn
        substituteRequests fb' up dn routine

substituteRequests
    :: forall self super x' x c' c b' b a'.
       ('[Bidir] <: self, Monad super)
    => (b -> Bi x' x c' c self super b')
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
                              let routine = runBi (fb' (unsafeCoerce b'))
                                                       (unsafeCoerce up)
                                                       (unsafeCoerce dn)
                              res <- routine
                              let continue = k (unsafeCoerce res)
                              transform go continue
                      _ -> ignore
              _ -> ignore

infixr 5 /</
(/</) :: ('[Bidir] <: self, Monad super)
      => (c' -> Bi b' b x' x self super c)
      -> (b' -> Bi a' a x' x self super b)
      -> c'
      -> Bi a' a x' x self super c
p1 /</ p2 = p2 \>\ p1

infixr 5 >~
(>~) :: ('[Bidir] <: self, Monad super)
     => Bi a' a y' y self super b
     -> Bi () b y' y self super c
     -> Bi a' a y' y self super c
p1 >~ p2 = (\() -> p1) >\\ p2

infixl 5 ~<
(~<) :: ('[Bidir] <: self, Monad super)
     => Bi () b y' y self super c
     -> Bi a' a y' y self super b
     -> Bi a' a y' y self super c
p2 ~< p1 = p1 >~ p2

infixl 5 \>\
(\>\) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a y' y self super b)
      -> (c' -> Bi b' b y' y self super c)
      -> c'
      -> Bi a' a y' y self super c
(fb' \>\ fc') c' = fb' >\\ fc' c'

infixl 4 \\<
(\\<) :: ('[Bidir] <: self, Monad super)
      => Bi b' b y' y self super c
      -> (b' -> Bi a' a y' y self super b)
      -> Bi a' a y' y self super c
p \\< f = f >\\ p

--------------------------------------------------------------------------------
-- Push; substitute responds with requests

infixl 7 >>~
(>>~)
    :: forall self a' a b' b c' c super r.
       (Monad super, '[Bidir] <: self)
    => Bi a' a b' b self super r
    -> (b -> Bi b' b c' c self super r)
    -> Bi a' a c' c self super r
p0 >>~ fb0 =
    Bi $ \up dn -> do
        pushRewrite up dn fb0 p0

pushRewrite
    :: forall self super r a' a b' b c' c.
       ('[Bidir] <: self, Monad super)
    => (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b -> Bi b' b c' c self super r)
    -> Bi a' a b' b self super r
    -> Narrative self super r
pushRewrite up dn fb0 p0 =
    let upstream = runBi p0 (unsafeCoerce up) (unsafeCoerce dn)
        downstream b = runBi (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)
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
(<~<) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi b' b c' c self super r)
      -> (a -> Bi a' a b' b self super r)
      -> a
      -> Bi a' a c' c self super r
p1 <~< p2 = p2 >~> p1

infixr 8 >~>
(>~>) :: ('[Bidir] <: self, Monad super)
      => (_a -> Bi a' a b' b self super r)
      -> (b -> Bi b' b c' c self super r)
      -> _a
      -> Bi a' a c' c self super r
(fa >~> fb) a = fa a >>~ fb

infixr 7 ~<<
(~<<) :: ('[Bidir] <: self, Monad super)
      => (b -> Bi b' b c' c self super r)
      -> Bi a' a b' b self super r
      -> Bi a' a c' c self super r
k ~<< p = p >>~ k

--------------------------------------------------------------------------------
-- Pull; substitute requests with responds

infixr 6 +>>
(+>>) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a b' b self super r)
      ->        Bi b' b c' c self super r
      ->        Bi a' a c' c self super r
fb' +>> p0 =
    Bi $ \up dn -> do
        pullRewrite up dn fb' p0

pullRewrite
    :: forall self super a' a b' b c' c r.
       ('[Bidir] <: self, Monad super)
    => (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b' -> Bi a' a b' b self super r)
    -> Bi b' b c' c self super r
    -> Narrative self super r
pullRewrite up dn fb' p =
    let upstream b' = runBi (fb' b') (unsafeCoerce up) (unsafeCoerce dn)
        downstream = runBi p (unsafeCoerce up) (unsafeCoerce dn)
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
(>->) :: ('[Bidir] <: self, Monad super)
      => Bi a' a () b self super r
      -> Bi () b c' c self super r
      -> Bi a' a c' c self super r
p1 >-> p2 = (\() -> p1) +>> p2

infixr 7 <-<
(<-<) :: ('[Bidir] <: self, Monad super)
      => Bi () b c' c self super r
      -> Bi a' a () b self super r
      -> Bi a' a c' c self super r
p2 <-< p1 = p1 >-> p2

infixr 7 <+<
(<+<) :: ('[Bidir] <: self, Monad super)
      => (c' -> Bi b' b c' c self super r)
      -> (b' -> Bi a' a b' b self super r)
      -> c'
      -> Bi a' a c' c self super r
p1 <+< p2 = p2 >+> p1

infixl 7 >+>
(>+>) :: ('[Bidir] <: self, Monad super)
      => (b' -> Bi a' a b' b self super r)
      -> (_c' -> Bi b' b c' c self super r)
      -> _c'
      -> Bi a' a c' c self super r
(fb' >+> fc') c' = fb' +>> fc' c'

infixl 6 <<+
(<<+) :: ('[Bidir] <: self, Monad super)
      => Bi b' b c' c self super r
      -> (b' -> Bi a' a b' b self super r)
      -> Bi a' a c' c self super r
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

{-# INLINE runBidir #-}
{-# INLINE closed #-}
{-# INLINE producer #-}
{-# INLINE consumer #-}
{-# INLINE line #-}
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
