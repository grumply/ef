{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}
{-# language ViewPatterns #-}
{-# language NoCPP #-}
module Ef.Sync
    ( Sync
    , sync
    , runSync

    , Producer
    , Producer'
    , producer

    , Consumer
    , Consumer'
    , consumer

    , Channel
    , channel

    , Client
    , Client'

    , Server
    , Server'

    , Synchronized(..)
    , synchronized
    , Effect
    , Effect'

    , X

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
    ) where

import Ef

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Unsafe.Coerce

-- Like Pipes with unique, scoped methods for flattened pipes transformers.
-- Should be equivalent to towers of Pipes transformers modulo the need to
-- keep the scoped methods local to their interpreters. This is generally a
-- lower-level construction used to implement other, safer interfaces, e.g.
-- the Exit module for short-circuiting continuations, or the Var module for
-- computation-local state, or the Note module for computation-local writers.

instance Delta Sync Sync where
    delta eval Sync {..} FreshScope {..} = delta eval scopeCreator createScope

data Sync k where
  Sync
    :: { scopeCreator :: (Int,k) } -> Sync k
  FreshScope
    :: { createScope :: Int -> k } -> Sync k
  Request
    :: Int -> a' -> (a -> k) -> Sync k
  Respond
    :: Int -> b -> (b' -> k) -> Sync k

instance Functor Sync where
  fmap f (Sync (i,k)) = Sync (i,f k)
  fmap f (FreshScope i) = FreshScope (fmap f i)
  fmap f (Request i a' ak) = Request i a' (fmap f ak)
  fmap f (Respond i b b'k) = Respond i b (fmap f b'k)

sync :: (Monad c, ts <. '[Sync]) => Sync (Action ts c)
sync = Sync (0,\o -> let Module Sync {..} _ = o
                     in pure $ Module (Sync { scopeCreator = first succ scopeCreator }) o
            )

freshScope :: (Monad c, ms <: '[Sync]) => Ef ms c Int
freshScope = Send (FreshScope Return)

getScope :: (ms <: '[Sync], Monad c) => Ef ms c a -> c Int
getScope (Send x) =
  case x of
    Request i _ _ -> return i
    Respond i _ _ -> return i

runSync :: (ms <: '[Sync], Monad c) => Effect ms c r -> Ef ms c r
runSync e = do
  scope <- freshScope
  rewrite scope $
      runSynchronized e
          (\a' apl -> Send (Request scope a' apl))
          (\b b'p -> Send (Respond scope b b'p))

rewrite :: forall ms c r. (ms <: '[Sync], Monad c) => Int -> Ef ms c r -> Ef ms c r
rewrite rewriteScope = transform id go
  where

    go :: Messages ms (Ef ms c r) -> Ef ms c r
    go message = do
        let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
            ignore = Do message
        case prj message of
               Just (Request currentScope a' _) -> check currentScope $ closed (unsafeCoerce a')
               Just (Respond currentScope b _) -> check currentScope $ closed (unsafeCoerce b)
               Nothing -> Do (fmap (transform id go) message)

instance (Monad c, Functor (Messages ms)) => Functor (Synchronized a' a b' b ms c) where
  fmap f (Synchronized w) = Synchronized $ \up dn -> fmap f (w up dn)

instance (Monad c, Functor (Messages ms)) => Applicative (Synchronized a' a b' b ms c) where
  pure a = Synchronized $ \_ _ -> pure a
  wf <*> wx = Synchronized $ \up dn -> do
    f <- runSynchronized wf up dn
    fmap f (runSynchronized wx (unsafeCoerce up) (unsafeCoerce dn))
  (*>) = (>>)

instance (Monad c, Functor (Messages ms)) => Monad (Synchronized a' a b' b ms c) where
  return = pure
  r >>= rs = Synchronized $ \up dn -> do
    v <- runSynchronized r (unsafeCoerce up) (unsafeCoerce dn)
    runSynchronized (rs v) up dn

instance ( Monad c, Functor (Messages ms), Monoid r) => Monoid (Synchronized a' a b' b ms c r) where
  mempty = pure mempty
  mappend w1 w2 = Synchronized $ \up dn -> do
    result <- runSynchronized w1 up dn
    fmap (mappend result) $ runSynchronized w2 (unsafeCoerce up) (unsafeCoerce dn)

instance (MonadPlus c, Monad c, Functor (Messages ms)) => Alternative (Synchronized a' a b' b ms c) where
  empty = mzero
  (<|>) = mplus

-- what does this look like without inspecting Lift since that was the entire
-- point of implementing 'transform'?
instance (MonadPlus c, Monad c, Functor (Messages ms)) => MonadPlus (Synchronized a' a b' b ms c) where
    mzero = Synchronized $ \_ _ -> super mzero
    mplus w0 w1 = Synchronized $ \up dn ->
      let routine = runSynchronized w0 (unsafeCoerce up) (unsafeCoerce dn)
      in rewriteMplus up dn routine
      where
        rewriteMplus up dn = go
          where

            go (Return r) =
                Return r

            go (Do sym) =
                Do (fmap go sym)

            go (Lift sup) =
              let
                routine =
                    runSynchronized w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                Lift (fmap go sup `mplus` return routine)

newtype X = X X

closed :: X -> a
closed (X x) = closed x

type Effect ms c r = Synchronized X () () X ms c r

type Producer b ms c r = Synchronized X () () b ms c r

producer :: forall ms c b r. (ms <: '[Sync], Monad c)
         => ((b -> Ef ms c ()) -> Ef ms c r) -> Producer' b ms c r
producer f = Synchronized $ \_ dn -> do
  let scopedDown = dn (unsafeCoerce ()) (unsafeCoerce ())
      respond :: Int -> b -> Ef ms ctx ()
      respond scope b = Send (Respond scope b Return)
  i <- lift (getScope scopedDown)
  f (respond i)

type Consumer a ms c r = Synchronized () a () X ms c r

consumer :: forall ms c a r. (ms <: '[Sync], Monad c)
         => (Ef ms c a -> Ef ms c r) -> Consumer' a ms c r
consumer f = Synchronized $ \up _ -> do
  let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
      request :: Int -> Ef ms ctx a
      request scope = Send (Request scope () Return)
  i <- lift (getScope scopedUp)
  f (request i)

type Channel a b ms c r = Synchronized () a () b ms c r

channel :: forall ms c a b r. (ms <: '[Sync], Monad c)
        => (Ef ms c a -> (b -> Ef ms c ()) -> Ef ms c r) -> Channel a b ms c r
channel f = Synchronized $ \up _ -> do
  let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
  i <- lift (getScope scopedUp)
  let request = Send (Request i () Return)
      respond b = Send (Respond i b Return)
  f request respond

type Client a' a ms c r = Synchronized a' a () X ms c r

type Server b' b ms c r = Synchronized X () b' b ms c r

newtype Synchronized a' a b' b ms c r =
    Synchronized
        {
          runSynchronized
              :: (forall x. a' -> (a -> Ef ms c x) -> Ef ms c x)
              -> (forall x. b -> (b' -> Ef ms c x) -> Ef ms c x)
              -> Ef ms c r
        }

synchronized :: forall a a' b b' ms c r. (ms <: '[Sync], Monad c)
             => ((a' -> Ef ms c a) -> (b -> Ef ms c b') -> Ef ms c r)
             -> Synchronized a' a b' b ms c r
synchronized f = Synchronized $ \up _ -> do
  let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
  i <- lift (getScope scopedUp)
  let request a = Send (Request i a Return)
      respond b' = Send (Respond i b' Return)
  f request respond

type Effect' ms c r = forall x' x y' y. Synchronized x' x y' y ms c r

type Producer' b ms c r = forall x' x. Synchronized x' x () b ms c r

type Consumer' a ms c r = forall y' y. Synchronized () a y' y ms c r

type Server' b' b ms c r = forall x' x. Synchronized x' x b' b ms c r

type Client' a' a ms c r = forall y' y. Synchronized a' a y' y ms c r

--------------------------------------------------------------------------------
-- Respond; substitute yields

cat :: (ms <: '[Sync], Monad c) => Channel a a ms c r
cat = channel $ \awt yld -> forever (awt >>= yld)

(//>) :: (ms <: '[Sync], Monad ctx)
      => Synchronized x' x b' b ms ctx a'
      -> (b -> Synchronized x' x c' c ms ctx b')
      -> Synchronized x' x c' c ms ctx a'
p0 //> fb =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        let routine = runSynchronized p0 up (unsafeCoerce dn)
        substituteResponds fb i up dn routine

substituteResponds
    :: forall ms ctx x' x c' c b' b a'.
       (ms <: '[Sync], Monad ctx)
    => (b -> Synchronized x' x c' c ms ctx b')
    -> Int
    -> (forall r. x' -> (x -> Ef ms ctx r) -> Ef ms ctx r)
    -> (forall r. c -> (c' -> Ef ms ctx r) -> Ef ms ctx r)
    -> Ef ms ctx a'
    -> Ef ms ctx a'
substituteResponds fb rewriteScope up dn =
    transform id go
  where

    go :: Messages ms (Ef ms ctx a') -> Ef ms ctx a'
    go message =
        case prj message of
            Just (Respond currentScope b k) ->
                if currentScope == rewriteScope
                then do
                  res <- runSynchronized (fb (unsafeCoerce b)) (unsafeCoerce up) (unsafeCoerce dn)
                  transform id go (unsafeCoerce k res)
                else Do (fmap (transform id go) message)
            _ -> Do (fmap (transform id go) message)

for :: (ms <: '[Sync], Monad ctx)
    => Synchronized x' x b' b ms ctx a'
    -> (b -> Synchronized x' x c' c ms ctx b')
    -> Synchronized x' x c' c ms ctx a'
for = (//>)

(<\\) :: (ms <: '[Sync], Monad ctx)
      => (b -> Synchronized x' x c' c ms ctx b')
      -> Synchronized x' x b' b ms ctx a'
      -> Synchronized x' x c' c ms ctx a'
f <\\ p = p //> f

(\<\) :: (ms <: '[Sync], Monad ctx)
      => (b -> Synchronized x' x c' c ms ctx b')
      -> (a -> Synchronized x' x b' b ms ctx a')
      -> a
      -> Synchronized x' x c' c ms ctx a'
p1 \<\ p2 = p2 />/ p1

(~>) :: (ms <: '[Sync], Monad ctx)
     => (a -> Synchronized x' x b' b ms ctx a')
     -> (b -> Synchronized x' x c' c ms ctx b')
     -> a
     -> Synchronized x' x c' c ms ctx a'
(~>) = (/>/)

(<~) :: (ms <: '[Sync], Monad ctx)
     => (b -> Synchronized x' x c' c ms ctx b')
     -> (a -> Synchronized x' x b' b ms ctx a')
     -> a
     -> Synchronized x' x c' c ms ctx a'
g <~ f = f ~> g

(/>/) :: (ms <: '[Sync], Monad ctx)
      => (a -> Synchronized x' x b' b ms ctx a')
      -> (b -> Synchronized x' x c' c ms ctx b')
      -> a
      -> Synchronized x' x c' c ms ctx a'
(fa />/ fb) a = fa a //> fb

--------------------------------------------------------------------------------
-- Request; substitute awaits

(>\\) :: (ms <: '[Sync], Monad ctx)
      => (b' -> Synchronized a' a y' y ms ctx b)
      -> Synchronized b' b y' y ms ctx c
      -> Synchronized a' a y' y ms ctx c
fb' >\\ p0 =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        let routine = runSynchronized p0 (unsafeCoerce up) dn
        substituteRequests fb' i up dn routine

substituteRequests
    :: forall ms ctx x' x c' c b' b a'.
       (ms <: '[Sync], Monad ctx)
    => (b -> Synchronized x' x c' c ms ctx b')
    -> Int
    -> (forall r. x' -> (x -> Ef ms ctx r) -> Ef ms ctx r)
    -> (forall r. c -> (c' -> Ef ms ctx r) -> Ef ms ctx r)
    -> Ef ms ctx a'
    -> Ef ms ctx a'
substituteRequests fb' rewriteScope up dn =
    transform id go
  where

    go :: Messages ms (Ef ms ctx a') -> Ef ms ctx a'
    go message =
        case prj message of
            Just (Request currentScope b' k) -> do
                if currentScope == rewriteScope
                then do
                    res <- runSynchronized (fb' (unsafeCoerce b')) (unsafeCoerce up) (unsafeCoerce dn)
                    transform id go (unsafeCoerce k res)
                else Do (fmap (transform id go) message)
            _ -> Do (fmap (transform id go) message)



(/</) :: (ms <: '[Sync], Monad ctx)
      => (c' -> Synchronized b' b x' x ms ctx c)
      -> (b' -> Synchronized a' a x' x ms ctx b)
      -> c'
      -> Synchronized a' a x' x ms ctx c
p1 /</ p2 = p2 \>\ p1

(>~) :: (ms <: '[Sync], Monad ctx)
     => Synchronized a' a y' y ms ctx b
     -> Synchronized () b y' y ms ctx c
     -> Synchronized a' a y' y ms ctx c
p1 >~ p2 = (\() -> p1) >\\ p2

(~<) :: (ms <: '[Sync], Monad ctx)
     => Synchronized () b y' y ms ctx c
     -> Synchronized a' a y' y ms ctx b
     -> Synchronized a' a y' y ms ctx c
p2 ~< p1 = p1 >~ p2

(\>\) :: (ms <: '[Sync], Monad ctx)
      => (b' -> Synchronized a' a y' y ms ctx b)
      -> (c' -> Synchronized b' b y' y ms ctx c)
      -> c'
      -> Synchronized a' a y' y ms ctx c
(fb' \>\ fc') c' = fb' >\\ fc' c'

(//<) :: (ms <: '[Sync], Monad ctx)
      => Synchronized b' b y' y ms ctx c
      -> (b' -> Synchronized a' a y' y ms ctx b)
      -> Synchronized a' a y' y ms ctx c
p //< f = f >\\ p

--------------------------------------------------------------------------------
-- Push; substitute responds with requests

(>>~)
    :: forall ms a' a b' b c' c ctx r.
       (Monad ctx, ms <: '[Sync])
    => Synchronized a' a b' b ms ctx r
    -> (b -> Synchronized b' b c' c ms ctx r)
    -> Synchronized a' a c' c ms ctx r
p0 >>~ fb0 =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        pushRewrite i up dn fb0 p0

pushRewrite
    :: forall ms ctx r a' a b' b c' c.
       (ms <: '[Sync], Monad ctx)
    => Int
    -> (forall x. a' -> (a -> Ef ms ctx x) -> Ef ms ctx x)
    -> (forall x. c -> (c' -> Ef ms ctx x) -> Ef ms ctx x)
    -> (b -> Synchronized b' b c' c ms ctx r)
    -> Synchronized a' a b' b ms ctx r
    -> Ef ms ctx r
pushRewrite rewriteScope up dn fb0 p0 =
    let upstream = runSynchronized p0 (unsafeCoerce up) (unsafeCoerce dn)
        downstream b = runSynchronized (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)
    in goLeft downstream upstream
  where
    goLeft fb =
      transform id goLeft'
      where
        goLeft' :: Messages ms (Ef ms ctx r) -> Ef ms ctx r
        goLeft' message =
          let ignore = Do (fmap (transform id goLeft') message)
          in case prj message of
              Just x ->
                case x of
                  Respond scope b k ->
                    if scope == rewriteScope
                    then goRight (unsafeCoerce k) (fb (unsafeCoerce b))
                    else ignore
                  _ -> ignore
              _ -> ignore

    goRight b'p =
      transform id goRight'
      where
        goRight' :: Messages ms (Ef ms ctx r) -> Ef ms ctx r
        goRight' message =
          let ignore = Do (fmap (transform id goRight') message)
          in case prj message of
              Just x  ->
                case x of
                  Request scope b' k ->
                    if scope == rewriteScope
                    then goLeft (unsafeCoerce k) (b'p (unsafeCoerce b'))
                    else ignore
                  _ -> ignore
              _ -> ignore

(<~<) :: (ms <: '[Sync], Monad ctx)
      => (b -> Synchronized b' b c' c ms ctx r)
      -> (a -> Synchronized a' a b' b ms ctx r)
      -> a
      -> Synchronized a' a c' c ms ctx r
p1 <~< p2 = p2 >~> p1

(>~>) :: (ms <: '[Sync], Monad ctx)
      => (_a -> Synchronized a' a b' b ms ctx r)
      -> (b -> Synchronized b' b c' c ms ctx r)
      -> _a
      -> Synchronized a' a c' c ms ctx r
(fa >~> fb) a = fa a >>~ fb

(~<<) :: (ms <: '[Sync], Monad ctx)
      => (b -> Synchronized b' b c' c ms ctx r)
      -> Synchronized a' a b' b ms ctx r
      -> Synchronized a' a c' c ms ctx r
k ~<< p = p >>~ k

--------------------------------------------------------------------------------
-- Pull; substitute requests with responds

(+>>) :: (ms <: '[Sync], Monad ctx)
      => (b' -> Synchronized a' a b' b ms ctx r)
      ->        Synchronized b' b c' c ms ctx r
      ->        Synchronized a' a c' c ms ctx r
fb' +>> p0 =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        pullRewrite i up dn fb' p0

pullRewrite
    :: forall ms ctx a' a b' b c' c r.
       (ms <: '[Sync], Monad ctx)
    => Int
    -> (forall x. a' -> (a -> Ef ms ctx x) -> Ef ms ctx x)
    -> (forall x. c -> (c' -> Ef ms ctx x) -> Ef ms ctx x)
    -> (b' -> Synchronized a' a b' b ms ctx r)
    -> Synchronized b' b c' c ms ctx r
    -> Ef ms ctx r
pullRewrite rewriteScope up dn fb' p =
    let upstream b' = runSynchronized (fb' b') (unsafeCoerce up) (unsafeCoerce dn)
        downstream = runSynchronized p (unsafeCoerce up) (unsafeCoerce dn)
    in goRight upstream downstream
  where
    goRight fb'' =
      transform id goRight'
      where
        goRight' :: Messages ms (Ef ms ctx r) -> Ef ms ctx r
        goRight' message =
          let ignore = Do (fmap (transform id goRight') message)
          in case prj message of
              Just x ->
                case x of
                  Request scope b' k ->
                    if scope == rewriteScope
                    then goLeft (unsafeCoerce k) (fb'' (unsafeCoerce b'))
                    else ignore
                  _ -> ignore
              _ -> ignore

    goLeft bp =
      transform id goLeft'
      where
        goLeft' :: Messages ms (Ef ms ctx r) -> Ef ms ctx r
        goLeft' message =
            let ignore = Do (fmap (transform id goLeft') message)
            in case prj message of
                Just x ->
                  case x of
                    Respond scope b k ->
                      if scope == rewriteScope
                      then goRight (unsafeCoerce k) (bp (unsafeCoerce b))
                      else ignore
                    _ -> ignore
                _ -> ignore

(>->) :: (ms <: '[Sync], Monad ctx)
      => Synchronized a' a () b ms ctx r
      -> Synchronized () b c' c ms ctx r
      -> Synchronized a' a c' c ms ctx r
p1 >-> p2 = (\() -> p1) +>> p2

(<-<) :: (ms <: '[Sync], Monad ctx)
      => Synchronized () b c' c ms ctx r
      -> Synchronized a' a () b ms ctx r
      -> Synchronized a' a c' c ms ctx r
p2 <-< p1 = p1 >-> p2

(<+<) :: (ms <: '[Sync], Monad ctx)
      => (c' -> Synchronized b' b c' c ms ctx r)
      -> (b' -> Synchronized a' a b' b ms ctx r)
      -> c'
      -> Synchronized a' a c' c ms ctx r
p1 <+< p2 = p2 >+> p1

(>+>) :: (ms <: '[Sync], Monad ctx)
      => (b' -> Synchronized a' a b' b ms ctx r)
      -> (_c' -> Synchronized b' b c' c ms ctx r)
      -> _c'
      -> Synchronized a' a c' c ms ctx r
(fb' >+> fc') c' = fb' +>> fc' c'

(<<+) :: (ms <: '[Sync], Monad ctx)
      => Synchronized b' b c' c ms ctx r
      -> (b' -> Synchronized a' a b' b ms ctx r)
      -> Synchronized a' a c' c ms ctx r
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

  ; "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

  ; "f >~ (g >~ p)" forall f g p . f >~ (g >~ p) = (f >~ g) >~ p

  ; "p1 >-> (p2 >-> p3)" forall p1 p2 p3 .
        p1 >-> (p2 >-> p3) = (p1 >-> p2) >-> p3

  ; "p >-> cat" forall p . p >-> cat = p

  ; "cat >-> p" forall p . cat >-> p = p

  #-}


{-# INLINE runSync #-}
{-# INLINE closed #-}
{-# INLINE producer #-}
{-# INLINE consumer #-}
{-# INLINE channel #-}
{-# INLINE synchronized #-}

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
{-# INLINE (//<) #-}

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
