-- This is a port of Gabriel Gonzalez's BSD-3 licensed pipes library: https://hackage.haskell.org/package/pipes
{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing -fno-warn-missing-methods #-}
{-# language DeriveFunctor #-}
{-# language DeriveAnyClass #-}
{-# language RankNTypes #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language InstanceSigs #-}
{-# language NoCPP #-}
module Ef.Pipes where

import Ef hiding (Proxy,pull,push)
import Data.Foldable as F

import Unsafe.Coerce

data Pipes a' a b' b k
  = Request a' (a -> k)
  | Respond b  (b' -> k)
  deriving Functor

newtype X = X X

{-# INLINABLE runEffect #-}
runEffect :: Monad c => Effect c r -> c r
runEffect = foldn return join (error "runEffect: error")

type Effect = Narrative (Pipes X () () X)

type Producer b = Narrative (Pipes X () () b)

type Consumer a = Narrative (Pipes () a () X)

type Pipe a b = Narrative (Pipes () a () b)

type Client a' a = Narrative (Pipes a' a () X)

type Server b' b = Narrative (Pipes X () b' b)

type Effect' m r = forall x' x y' y. Narrative (Pipes x' x y' y) m r

type Producer' b m r = forall x' x. Narrative (Pipes x' x () b) m r

type Consumer' a m r = forall y' y. Narrative (Pipes () a y' y) m r

type Server' b' b m r = forall x' x. Proxy x' x b' b m r

type Client' a' a m r = forall y' y. Narrative (Pipes a' a y' y) m r

type Proxy a' a b' b = Narrative (Pipes a' a b' b)

{-# INLINABLE [1] request #-}
request :: forall a' a y' y m. Monad m => a' -> Proxy a' a y' y m a
request a' = send (Request a' id)

{-# INLINABLE [1] respond #-}
respond :: forall a' a x' x m. Monad m => a -> Proxy x' x a' a m a'
respond b = send (Respond b id)

{-# INLINABLE [1] await #-}
await :: Monad m => Consumer' a m a
await = send (Request () id)

{-# INLINABLE [1] yield #-}
yield :: Monad m => a -> Producer' a m ()
yield a = send (Respond a id)

{-# INLINABLE [1] pull #-}
pull :: Monad m => a' -> Proxy a' a a' a m r
pull = pull'
  where
    pull' a' = Do (Request a' (\a -> Do (Respond a pull')))

{-# INLINABLE [1] push #-}
push :: Monad m => a -> Proxy a' a a' a m r
push = push'
  where
    push' a = Do (Respond a (\a' -> Do (Request a' push')))

closed :: X -> a
closed (X x) = closed x

--------------------------------------------------------------------------------
-- ListT

newtype ListT m a = Select { enumerate :: Producer a m () }

instance Monad m => Functor (ListT m) where
  fmap f p = Select (for (enumerate p) (\a -> yield (f a)))

instance Monad m => Applicative (ListT m) where
  pure a = Select (yield a)
  mf <*> mx = Select (
    for (enumerate mf) (\f ->
      for (enumerate mx) (\x ->
        yield (f x))))

instance Monad m => Monad (ListT m) where
  return a = Select (yield a)
  m >>= f = Select $ for (enumerate m) (enumerate . f)
  fail _ = mzero

instance Monad m => Alternative (ListT m) where
  empty = Select (return ())
  p1 <|> p2 =
    Select $ do
      enumerate p1
      enumerate p2

instance Monad m => MonadPlus (ListT m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => Monoid (ListT m a) where
  mempty = empty
  mappend = (<>)

instance Monad m => Semigroup (ListT m a) where
  (<>) = (<|>)

instance (Foldable m) => Foldable (ListT m) where
    foldMap :: forall a x. Monoid x => (a -> x) -> ListT m a -> x
    foldMap f = go . enumerate
      where
        go :: Producer a m () -> x
        go p =
          case p of
            Return _ -> mempty
            Lift sup -> F.foldMap go sup
            Do (Respond a fu) -> f a `mappend` go (fu ())
    {-# INLINE [1] foldMap #-}

instance MonadTrans ListT where
  lift m = Select (lift m >>= yield)

instance MonadIO m => MonadIO (ListT m) where
  liftIO m = Select (lift (liftIO m) >>= yield)

next :: forall a m r. Monad m => Producer a m r -> m (Either r (a,Producer a m r))
next = go
  where
    go :: Producer a m r -> m (Either r (a,Producer a m r))
    go p =
      case p of
        Return r -> return (Left r)
        Lift sup -> sup >>= go
        Do (Request x _) -> closed x
        Do (Respond a fu) -> return (Right (unsafeCoerce a,unsafeCoerce fu ()))

runListT :: Monad m => ListT m a -> m ()
runListT l = runEffect (enumerate (l >> mzero))

each :: (Monad m, F.Foldable f) => f a -> Producer' a m ()
each xs = F.foldr (\a p -> yield a >> p) (return ()) xs

discard :: Monad m => t -> Narrative (Pipes a' a b' b) m ()
discard _ = return ()

every :: Monad m => ListT m a -> Producer' a m ()
every it = discard >\\ enumerate it

--------------------------------------------------------------------------------
-- Respond; substitute yields

{-# INLINE [1] cat #-}
cat :: Monad m => Pipe a a m r
cat = forever (await >>= yield)

{-# INLINE [1] (//>) #-}
(//>) :: forall x' x b' b a' c' c m. Monad m
      => Proxy x' x b' b m a'
      -> (b -> Proxy x' x c' c m b')
      -> Proxy x' x c' c m a'
(//>) p0 fb = foldn Return Lift msg p0
  where
    msg m =
      case m of
        Request x' fx -> Do (Request x' fx)
        Respond b fb' -> fb b >>= \b' -> fb' b'

{-# INLINE [1] for #-}
for :: Monad m
    => Proxy x' x b' b m a'
    -> (b -> Proxy x' x c' c m b')
    -> Proxy x' x c' c m a'
for = (//>)

{-# INLINE [1] (<\\) #-}
(<\\) :: Monad m
      => (b -> Proxy x' x c' c m b')
      -> Proxy x' x b' b m a'
      -> Proxy x' x c' c m a'
f <\\ p = p //> f

{-# INLINABLE [1] (\<\) #-}
(\<\) :: Monad m
      => (b -> Proxy x' x c' c m b')
      -> (a -> Proxy x' x b' b m a')
      -> a
      -> Proxy x' x c' c m a'
p1 \<\ p2 = p2 />/ p1

{-# INLINE [1] (~>) #-}
(~>) :: Monad m
     => (a -> Proxy x' x b' b m a')
     -> (b -> Proxy x' x c' c m b')
     -> a
     -> Proxy x' x c' c m a'
(~>) = (/>/)

{-# INLINE [1] (<~) #-}
(<~) :: Monad m
     => (b -> Proxy x' x c' c m b')
     -> (a -> Proxy x' x b' b m a')
     -> a
     -> Proxy x' x c' c m a'
g <~ f = f ~> g

{-# INLINABLE [1] (/>/) #-}
(/>/) :: Monad m
      => (a -> Proxy x' x b' b m a')
      -> (b -> Proxy x' x c' c m b')
      -> (a -> Proxy x' x c' c m a')
(fa />/ fb) a = fa a //> fb

--------------------------------------------------------------------------------
-- Request; substitute awaits

{-# INLINE [1] (>\\) #-}
(>\\) :: forall b b' a a' y y' c m. Monad m
      => (b' -> Proxy a' a y' y m b)
      -> Proxy b' b y' y m c
      -> Proxy a' a y' y m c
(>\\) fb' p = foldn Return Lift msg p
  where
    msg m =
      case m of
        Request b' fb -> fb' b' >>= \b -> fb b
        Respond x fx' -> Do (Respond x fx')

{-# INLINABLE [1] (/</) #-}
(/</) :: Monad m
      => (c' -> Proxy b' b x' x m c)
      -> (b' -> Proxy a' a x' x m b)
      -> c'
      -> Proxy a' a x' x m c
p1 /</ p2 = p2 \>\ p1

{-# INLINE [1] (>~) #-}
(>~) :: Monad m
     => Proxy a' a y' y m b
     -> Proxy () b y' y m c
     -> Proxy a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2

{-# INLINE [1] (~<) #-}
(~<) :: Monad m
     => Proxy () b y' y m c
     -> Proxy a' a y' y m b
     -> Proxy a' a y' y m c
p2 ~< p1 = p1 >~ p2

{-# INLINABLE [1] (\>\) #-}
(\>\) :: Monad m
      => (b' -> Proxy a' a y' y m b)
      -> (c' -> Proxy b' b y' y m c)
      -> c'
      -> Proxy a' a y' y m c
(fb' \>\ fc') c' = fb' >\\ fc' c'

{-# INLINABLE [1] (//<) #-}
(//<) :: Monad m
      => Proxy b' b y' y m c
      -> (b' -> Proxy a' a y' y m b)
      -> Proxy a' a y' y m c
p //< f = f >\\ p

--------------------------------------------------------------------------------
-- Push; substitute responds with requests

{-# INLINE [1] (>>~) #-}
(>>~)
    :: forall a' a b' b c' c m r.
       Monad m
    => Proxy a' a b' b m r
    -> (b -> Proxy b' b c' c m r)
    -> Proxy a' a c' c m r
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


{-# INLINABLE [1] (<~<) #-}
(<~<) :: Monad m
      => (b -> Proxy b' b c' c m r)
      -> (a -> Proxy a' a b' b m r)
      -> a
      -> Proxy a' a c' c m r
p1 <~< p2 = p2 >~> p1

{-# INLINABLE [1] (>~>) #-}
(>~>) :: Monad m
      => (_a -> Proxy a' a b' b m r)
      -> (b -> Proxy b' b c' c m r)
      -> _a
      -> Proxy a' a c' c m r
(fa >~> fb) a = fa a >>~ fb

{-# INLINABLE [1] (~<<) #-}
(~<<) :: Monad m
      => (b -> Proxy b' b c' c m r)
      -> Proxy a' a b' b m r
      -> Proxy a' a c' c m r
k ~<< p = p >>~ k

--------------------------------------------------------------------------------
-- Pull; substitute requests with responds

{-# INLINABLE [1] (+>>) #-}
(+>>) :: forall a' a b' b c' c m r. Monad m
      => (b' -> Proxy a' a b' b m r)
      ->        Proxy b' b c' c m r
      ->        Proxy a' a c' c m r
fb' +>> p =
  case p of
    Do m ->
      case m of
        Request b' fb -> fb' b' >>~ fb
        Respond c fc' -> Do (Respond c (\c' -> fb' +>> fc' c'))
    Lift m            -> Lift (m >>= \p' -> return (fb' +>> p'))
    Return a          -> Return a

{-# RULES
  "(+>>) _ (Return r)" forall r fb'. fb' +>> (Return r) = Return r;
  "(+>>) _ (Lift m)" forall m fb'. fb' +>> (Lift m) = Lift (m >>= \p' -> return (fb' +>> p'));
  "(+>>) _ (Request b' fb)" forall fb' b' fb. fb' +>> (Do (Request b' fb)) = fb' b' >>~ fb;
  "(+>>) _ (Respond c fc')" forall fb' c fc'. fb' +>> (Do (Respond c fc')) = Do (Respond c (\c' -> fb' +>> fc' c'));
  #-}

{-# INLINE [1] (>->) #-}
(>->) :: Monad m
      => Proxy a' a () b m r
      -> Proxy () b c' c m r
      -> Proxy a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2

{-# INLINE [1] (<-<) #-}
(<-<) :: Monad m
      => Proxy () b c' c m r
      -> Proxy a' a () b m r
      -> Proxy a' a c' c m r
p2 <-< p1 = p1 >-> p2

{-# INLINABLE [1] (<+<) #-}
(<+<) :: Monad m
      => (c' -> Proxy b' b c' c m r)
      -> (b' -> Proxy a' a b' b m r)
      -> c'
      -> Proxy a' a c' c m r
p1 <+< p2 = p2 >+> p1

{-# INLINABLE [1] (>+>) #-}
(>+>) :: Monad m
      => (b' -> Proxy a' a b' b m r)
      -> (_c' -> Proxy b' b c' c m r)
      -> _c'
      -> Proxy a' a c' c m r
(fb' >+> fc') c' = fb' +>> fc' c'

{-# INLINABLE [1] (<<+) #-}
(<<+) :: Monad m
      => Proxy b' b c' c m r
      -> (b' -> Proxy a' a b' b m r)
      -> Proxy a' a c' c m r
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


{-# INLINE [1] each #-}
{-# INLINE [1] discard #-}
{-# INLINE [1] every #-}
{-# INLINE [1] next #-}

infixr 8 >~>
infixr 7 ~<<
infixr 6 +>>
infixl 7 >->
infixr 7 <-<
infixr 7 <+<
infixl 7 >+>
infixl 6 <<+
infixl 8 <~<
infixl 7 >>~
infixl 5 \>\
infixl 5 ~<
infixr 5 >~
infixr 5 /</
infixl 4 //<
infixr 4 >\\
infixr 4 />/
infixl 4 <~
infixr 4 ~>
infixl 4 \<\
infixl 3 //>
infixr 3 <\\
