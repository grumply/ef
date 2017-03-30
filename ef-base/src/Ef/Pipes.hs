-- This is a port of Gabriel Gonzalez's BSD-3 licensed pipes library: https://hackage.haskell.org/package/pipes
{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing -fno-warn-missing-methods #-}
{-# language DeriveFunctor #-}
{-# language DeriveAnyClass #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language InstanceSigs #-}
module Ef.Pipes
    ( Pipes(..)

    , Effect
    , Effect'
    , runEffect

    -- , request
    -- , respond
    -- , yield
    -- , await


    , Producer
    , Producer'

    , Consumer
    , Consumer'

    , Pipe

    , Client
    , Client'

    , Server
    , Server'

    , request
    , respond
    , yield
    , await
    , pull
    , push

    , X
    , closed

    , (<\\)
    , (\<\)
    , (~>)
    , (<~)
    , (/>/)
    , (//>)

    , (//<)
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

    , ListT(..)
    , next
    , each
    , every
    , discard
    ) where

import Ef hiding (Proxy,pull,push)
import Data.Foldable as F

import Unsafe.Coerce

data Pipes a' a b' b k
  = Request a' (a -> k)
  | Respond b  (b' -> k)
  deriving Functor

newtype X = X X

{-# INLINE runEffect #-}
runEffect :: Monad c => Effect c r -> c r
runEffect = foldn return join (error "runEffect: error")

type Effect super r = Narrative (Pipes X () () X) super r

type Producer b super r = Narrative (Pipes X () () b) super r

type Consumer a super r = Narrative (Pipes () a () X) super r

type Pipe a b super r = Narrative (Pipes () a () b) super r

type Client a' a super r = Narrative (Pipes a' a () X) super r

type Server b' b super r = Narrative (Pipes X () b' b) super r

type Effect' super r = forall x' x y' y. Narrative (Pipes x' x y' y) super r

type Producer' b super r = forall x' x. Narrative (Pipes x' x () b) super r

type Consumer' a super r = forall y' y. Narrative (Pipes () a y' y) super r

type Server' b' b super r = forall x' x. Proxy x' x b' b super r

type Client' a' a super r = forall y' y. Narrative (Pipes a' a y' y) super r

type Proxy a' a b' b super r = Narrative (Pipes a' a b' b) super r

{-# INLINE request #-}
request :: forall a' a y' y super. Monad super => a' -> Proxy a' a y' y super a
request a' = Do (Request a' Return)

{-# INLINE respond #-}
respond :: forall a' a x' x super. Monad super => a -> Proxy x' x a' a super a'
respond b = Do (Respond b Return)

{-# INLINE await #-}
await :: Monad super => Consumer' a super a
await = Do (Request () Return)

{-# INLINE yield #-}
yield :: Monad super => a -> Producer' a super ()
yield a = Do (Respond a Return)


{-# INLINE pull #-}
pull :: Monad super => a' -> Proxy a' a a' a super r
pull = pull'
  where
    pull' a' = Do (Request a' (\a -> Do (Respond a pull')))

{-# INLINE push #-}
push :: Monad m => a -> Proxy a' a a' a m r
push = push'
  where
    push' a = Do (Respond a (\a' -> Do (Request a' push')))

closed :: X -> a
closed (X x) = closed x

--------------------------------------------------------------------------------
-- ListT

newtype ListT super a = Select { enumerate :: Producer a super () }

instance Monad super => Functor (ListT super) where
  fmap f p = Select (for (enumerate p) (\a -> yield (f a)))

instance Monad super => Applicative (ListT super) where
  pure a = Select (yield a)
  mf <*> mx = Select (
    for (enumerate mf) (\f ->
      for (enumerate mx) (\x ->
        yield (f x))))

instance Monad super => Monad (ListT super) where
  return a = Select (yield a)
  m >>= f = Select $ for (enumerate m) (enumerate . f)
  fail _ = mzero

instance Monad super => Alternative (ListT super) where
  empty = Select (return ())
  p1 <|> p2 =
    Select $ do
      enumerate p1
      enumerate p2

instance Monad super => MonadPlus (ListT super) where
  mzero = empty
  mplus = (<|>)

instance Monad super => Monoid (ListT super a) where
  mempty = empty
  mappend = (<|>)

instance (Foldable super) => Foldable (ListT super) where
    foldMap :: forall a m. Monoid m => (a -> m) -> ListT super a -> m
    foldMap f = go . enumerate
      where
        go :: Producer a super () -> m
        go p =
          case p of
            Return _ -> mempty
            Lift sup -> F.foldMap go sup
            Do (Respond a fu) -> f a `mappend` go (fu ())
    {-# INLINE foldMap #-}

instance MonadTrans ListT where
  lift m = Select (lift m >>= yield)

instance MonadIO m => MonadIO (ListT m) where
  liftIO m = Select (lift (liftIO m) >>= yield)

next :: forall a super r. Monad super => Producer a super r -> super (Either r (a,Producer a super r))
next = go
  where
    go :: Producer a super r -> super (Either r (a,Producer a super r))
    go p =
      case p of
        Return r -> return (Left r)
        Lift sup -> sup >>= go
        Do (Request x _) -> closed x
        Do (Respond a fu) -> return (Right (unsafeCoerce a,unsafeCoerce fu ()))

runListT :: Monad super => ListT super a -> super ()
runListT l = runEffect (enumerate (l >> mzero))

each :: (Monad super, F.Foldable f) => f a -> Producer' a super ()
each xs = F.foldr (\a p -> yield a >> p) (return ()) xs

discard :: Monad super => t -> Narrative (Pipes a' a b' b) super ()
discard _ = return ()

every :: Monad super => ListT super a -> Producer' a super ()
every it = discard >\\ enumerate it

--------------------------------------------------------------------------------
-- Respond; substitute yields

{-# INLINE cat #-}
cat :: Monad super => Pipe a a super r
cat = forever (await >>= yield)

{-# INLINE (//>) #-}
infixl 3 //>
(//>) :: forall x' x b' b a' c' c super. Monad super
      => Proxy x' x b' b super a'
      -> (b -> Proxy x' x c' c super b')
      -> Proxy x' x c' c super a'
(//>) p0 fb = foldn Return Lift msg p0
  where
    msg m =
      case m of
        Request x' fx -> Do (Request x' fx)
        Respond b fb' -> fb b >>= \b' -> fb' b'

{-# INLINE for #-}
for :: Monad super
    => Proxy x' x b' b super a'
    -> (b -> Proxy x' x c' c super b')
    -> Proxy x' x c' c super a'
for = (//>)

{-# INLINE (<\\) #-}
infixr 3 <\\
(<\\) :: Monad super
      => (b -> Proxy x' x c' c super b')
      -> Proxy x' x b' b super a'
      -> Proxy x' x c' c super a'
f <\\ p = p //> f

{-# INLINE (\<\) #-}
infixl 4 \<\
(\<\) :: Monad super
      => (b -> Proxy x' x c' c super b')
      -> (a -> Proxy x' x b' b super a')
      -> a
      -> Proxy x' x c' c super a'
p1 \<\ p2 = p2 />/ p1

{-# INLINE (~>) #-}
infixr 4 ~>
(~>) :: Monad super
     => (a -> Proxy x' x b' b super a')
     -> (b -> Proxy x' x c' c super b')
     -> a
     -> Proxy x' x c' c super a'
(~>) = (/>/)

{-# INLINE (<~) #-}
infixl 4 <~
(<~) :: Monad super
     => (b -> Proxy x' x c' c super b')
     -> (a -> Proxy x' x b' b super a')
     -> a
     -> Proxy x' x c' c super a'
g <~ f = f ~> g

{-# INLINE (/>/) #-}
infixr 4 />/
(/>/) :: Monad super
      => (a -> Proxy x' x b' b super a')
      -> (b -> Proxy x' x c' c super b')
      -> (a -> Proxy x' x c' c super a')
(fa />/ fb) a = fa a //> fb

--------------------------------------------------------------------------------
-- Request; substitute awaits

{-# INLINE (>\\) #-}
infixr 4 >\\
(>\\) :: forall b b' a a' y y' c super. Monad super
      => (b' -> Proxy a' a y' y super b)
      -> Proxy b' b y' y super c
      -> Proxy a' a y' y super c
(>\\) fb' p = foldn Return Lift msg p
  where
    msg m =
      case m of
        Request b' fb -> fb' b' >>= \b -> fb b
        Respond x fx' -> Do (Respond x fx')

{-# INLINE (/</) #-}
infixr 5 /</
(/</) :: Monad super
      => (c' -> Proxy b' b x' x super c)
      -> (b' -> Proxy a' a x' x super b)
      -> c'
      -> Proxy a' a x' x super c
p1 /</ p2 = p2 \>\ p1

{-# INLINE (>~) #-}
infixr 5 >~
(>~) :: Monad super
     => Proxy a' a y' y super b
     -> Proxy () b y' y super c
     -> Proxy a' a y' y super c
p1 >~ p2 = (\() -> p1) >\\ p2

{-# INLINE (~<) #-}
infixl 5 ~<
(~<) :: Monad super
     => Proxy () b y' y super c
     -> Proxy a' a y' y super b
     -> Proxy a' a y' y super c
p2 ~< p1 = p1 >~ p2

{-# INLINE (\>\) #-}
infixl 5 \>\
(\>\) :: Monad super
      => (b' -> Proxy a' a y' y super b)
      -> (c' -> Proxy b' b y' y super c)
      -> c'
      -> Proxy a' a y' y super c
(fb' \>\ fc') c' = fb' >\\ fc' c'

{-# INLINE (//<) #-}
infixl 4 //<
(//<) :: Monad super
      => Proxy b' b y' y super c
      -> (b' -> Proxy a' a y' y super b)
      -> Proxy a' a y' y super c
p //< f = f >\\ p

--------------------------------------------------------------------------------
-- Push; substitute responds with requests

{-# INLINE (>>~) #-}
infixl 7 >>~
(>>~)
    :: forall a' a b' b c' c super r.
       Monad super
    => Proxy a' a b' b super r
    -> (b -> Proxy b' b c' c super r)
    -> Proxy a' a c' c super r
p >>~ fb = 
  case p of
    Do m ->
      case m of
        Request a' fa -> Do (Request a' (\a -> fa a >>~ fb))
        Respond b fb' -> fb' +>> fb b
    Lift m -> Lift (m >>= \p' -> return (p' >>~ fb))
    Return a -> Return a

{-# RULES
  "(>>~) (Return r)" forall r fb. (Return r) >>~ fb = Return r;
  "(>>~) (Lift m)" forall m fb. (Lift m) >>~ fb = Lift (m >>= \p' -> return (p' >>~ fb));
  "(>>~) (Request a' fa)" forall a' fa fb. (Do (Request a' fa)) >>~ fb = Do (Request a' (\a -> fa a >>~ fb));
  "(>>~) (Respond b fb')" forall b fb' fb. (Do (Respond b fb')) >>~ fb = fb' +>> fb b;
  #-}


{-# INLINE (<~<) #-}
infixl 8 <~<
(<~<) :: Monad super
      => (b -> Proxy b' b c' c super r)
      -> (a -> Proxy a' a b' b super r)
      -> a
      -> Proxy a' a c' c super r
p1 <~< p2 = p2 >~> p1

{-# INLINE (>~>) #-}
infixr 8 >~>
(>~>) :: Monad super
      => (_a -> Proxy a' a b' b super r)
      -> (b -> Proxy b' b c' c super r)
      -> _a
      -> Proxy a' a c' c super r
(fa >~> fb) a = fa a >>~ fb

{-# INLINE (~<<) #-}
infixr 7 ~<<
(~<<) :: Monad super
      => (b -> Proxy b' b c' c super r)
      -> Proxy a' a b' b super r
      -> Proxy a' a c' c super r
k ~<< p = p >>~ k

--------------------------------------------------------------------------------
-- Pull; substitute requests with responds

{-# INLINE (+>>) #-}
infixr 6 +>>
(+>>) :: forall a' a b' b c' c super r. Monad super
      => (b' -> Proxy a' a b' b super r)
      ->        Proxy b' b c' c super r
      ->        Proxy a' a c' c super r
fb' +>> p =
  case p of
    Do m ->
      case m of
        Request b' fb -> fb' b' >>~ fb
        Respond c fc' -> Do (Respond c (\c' -> fb' +>> fc' c'))
    Lift m -> Lift (m >>= \p' -> return (fb' +>> p'))
    Return a -> Return a

{-# RULES
  "(+>>) _ (Return r)" forall r fb'. fb' +>> (Return r) = Return r;
  "(+>>) _ (Lift m)" forall m fb'. fb' +>> (Lift m) = Lift (m >>= \p' -> return (fb' +>> p'));
  "(+>>) _ (Request b' fb)" forall fb' b' fb. fb' +>> (Do (Request b' fb)) = fb' b' >>~ fb;
  "(+>>) _ (Respond c fc')" forall fb' c fc'. fb' +>> (Do (Respond c fc')) = Do (Respond c (\c' -> fb' +>> fc' c'));
  #-}

{-# INLINE (>->) #-}
infixl 7 >->
(>->) :: Monad super
      => Proxy a' a () b super r
      -> Proxy () b c' c super r
      -> Proxy a' a c' c super r
p1 >-> p2 = (\() -> p1) +>> p2

{-# INLINE (<-<) #-}
infixr 7 <-<
(<-<) :: Monad super
      => Proxy () b c' c super r
      -> Proxy a' a () b super r
      -> Proxy a' a c' c super r
p2 <-< p1 = p1 >-> p2

{-# INLINE (<+<) #-}
infixr 7 <+<
(<+<) :: Monad super
      => (c' -> Proxy b' b c' c super r)
      -> (b' -> Proxy a' a b' b super r)
      -> c'
      -> Proxy a' a c' c super r
p1 <+< p2 = p2 >+> p1

{-# INLINE (>+>) #-}
infixl 7 >+>
(>+>) :: Monad super
      => (b' -> Proxy a' a b' b super r)
      -> (_c' -> Proxy b' b c' c super r)
      -> _c'
      -> Proxy a' a c' c super r
(fb' >+> fc') c' = fb' +>> fc' c'

{-# INLINE (<<+) #-}
infixl 6 <<+
(<<+) :: Monad super
      => Proxy b' b c' c super r
      -> (b' -> Proxy a' a b' b super r)
      -> Proxy a' a c' c super r
p <<+ fb = fb +>> p

{-# RULES
    "(p //> f) //> g" forall p f g . (p //> f) //> g = p //> (\x -> f x //> g)
  ; "p //> respond" forall p . p //> respond = p
  ; "respond x //> f" forall x f . respond x //>  f = f x

  ; "f >\\ (g >\\ p)" forall f g p . f >\\ (g >\\ p) = (\x -> f >\\ g x) >\\ p
  ; "request >\\ p" forall p . request >\\ p = p
  ; "f >\\ request x" forall f x . f >\\ request x = f x

  ; "(p >>~ f) >>~ g" forall p f g . (p >>~ f) >>~ g = p >>~ (\x -> f x >>~ g)
  ; "p >>~ push" forall p . p >>~ push = p
  ; "push x >>~ f" forall x f . push x >>~ f = f x

  ; "f +>> (g +>> p)" forall f g p . f +>> (g +>> p) = (\x -> f +>> g x) +>> p
  ; "pull +>> p" forall p . pull +>> p = p
  ; "f +>> pull x" forall f x . f +>> pull x = f x

  ; "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

  ; "f >~ (g >~ p)" forall f g p . f >~ (g >~ p) = (f >~ g) >~ p

  ; "p1 >-> (p2 >-> p3)" forall p1 p2 p3 .
        p1 >-> (p2 >-> p3) = (p1 >-> p2) >-> p3

  ; "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

  ; "f >~ (g >~ p)" forall f g p . f >~ (g >~ p) = (f >~ g) >~ p

  ; "p1 >-> (p2 >-> p3)" forall p1 p2 p3 .
        p1 >-> (p2 >-> p3) = (p1 >-> p2) >-> p3

  ; "p >-> cat" forall p . p >-> cat = p

  ; "cat >-> p" forall p . cat >-> p = p

  ; "for p yield" forall p . for p yield = p

  ; "for (yield x) f" forall x f . for (yield x) f = f x

  ; "for cat f" forall f .
        for cat f =
            let go = do
                    x <- await
                    f x
                    go
            in  go

  ; "await >~ p" forall p . await >~ p = p

  ; "p >~ await" forall p . p >~ await = p

  ; "m >~ cat" forall m .
        m >~ cat =
            let go = do
                    x <- m
                    yield x
                    go
            in  go

  ; "p >-> cat" forall p . p >-> cat = p

  ; "cat >-> p" forall p . cat >-> p = p

  ; "_bind (Request a' k) f" forall a' k f .
        (Do (Request a' k)) >>= f = Do (Request a' (\a  -> (k a) >>= f))
  ; "_bind (Respond b  k) f" forall b  k f .
        (Do (Respond b  k)) >>= f = Do (Respond b  (\b' -> (k b') >>= f));

  ; "fb' >\\ (Do (Request b' fb))" forall fb' b' fb  .
      fb' >\\ (Do (Request b' fb)) = fb' b' >>= \b -> fb' >\\ fb  b

  ; "fb' >\\ (Do (Respond x fx'))" forall fb' x  fx'.
      fb' >\\ (Do (Respond x fx')) = Do (Respond x (\x' -> fb' >\\ fx' x'))

  ; "fb' >\\ (Lift m)" forall fb' m.
      fb' >\\ (Lift m) = Lift (m >>= \p' -> return (fb' >\\ p'))

  ; "fb' >\\ (Return a)" forall fb' a.
      fb' >\\ (Return a) = Return a

  ; "(Request x' fx ) //> fb" forall x' fx fb.
        Do (Request x' fx ) //> fb = Do (Request x' (\x -> fx x //> fb))

  ; "(Respond b fb') //> fb" forall b fb' fb .
        Do (Respond b  fb') //> fb = fb b >>= \b' -> fb' b' //> fb

  ; "(Lift m) //> fb" forall m fb.
        (Lift m) //> fb = Lift (m >>= \p' -> return (p' //> fb))

  ; "(Return a) //> fb" forall a fb.
        (Return a) //> fb = Return a

  #-}


{-# INLINE each #-}
{-# INLINE discard #-}
{-# INLINE every #-}
{-# INLINE next #-}
