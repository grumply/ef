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

sync :: (Monad c, '[Sync] <. ts) => Sync (Action ts c)
sync = Sync (0,\o -> let Module Sync {..} _ = o
                     in pure $ Module (Sync { scopeCreator = first succ scopeCreator }) o
            )

freshScope :: (Monad super, '[Sync] <: self) => Ef self super Int
freshScope = Send (FreshScope Return)

getScope :: ('[Sync] <: self, Monad super) => Ef self super a -> super Int
getScope (Send x) =
  case x of
      Request i _ _ -> return i
      Respond i _ _ -> return i

runSync :: ('[Sync] <: self, Monad super)
        => Effect self super r -> Ef self super r
runSync e = do
    scope <- freshScope
    rewrite scope $
        runSynchronized e
            (\a' apl -> Send (Request scope a' apl))
            (\b b'p -> Send (Respond scope b b'p))

rewrite :: forall self super result.
           ('[Sync] <: self, Monad super)
        => Int -> Ef self super result -> Ef self super result
rewrite rewriteScope = transform id go
  where

    go :: Messages self (Ef self super result) -> Ef self super result
    go message = do
        let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
            ignore = Do message
        case prj message of
               Just (Request currentScope a' _) -> check currentScope $ closed (unsafeCoerce a')
               Just (Respond currentScope b _) -> check currentScope $ closed (unsafeCoerce b)
               Nothing -> Do (fmap (transform id go) message)

instance (Monad super, Functor (Messages self))
    => Functor (Synchronized a' a b' b self super)
  where

    fmap f (Synchronized w) =
        Synchronized $ \up dn -> fmap f (w up dn)

instance (Monad super, Functor (Messages self))
    => Applicative (Synchronized a' a b' b self super)
  where

    pure a =
        Synchronized $ \_ _ -> pure a

    wf <*> wx =
        Synchronized $ \up dn -> do
            f <- runSynchronized wf up dn
            fmap f (runSynchronized wx (unsafeCoerce up) (unsafeCoerce dn))

    (*>) = (>>)

instance (Monad super, Functor (Messages self))
    => Monad (Synchronized a' a b' b self super)
  where

    return = pure

    r >>= rs =
        Synchronized $ \up dn -> do
            v <- runSynchronized r (unsafeCoerce up) (unsafeCoerce dn)
            runSynchronized (rs v) up dn

instance ( Monad super
         , Functor (Messages self)
         , Monoid r
         ) => Monoid (Synchronized a' a b' b self super r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        Synchronized $ \up dn -> do
            result <- runSynchronized w1 up dn
            fmap (mappend result) $ runSynchronized w2 (unsafeCoerce up) (unsafeCoerce dn)

instance (MonadPlus super, Monad super, Functor (Messages self))
    => Alternative (Synchronized a' a b' b self super)
  where

    empty = mzero

    (<|>) = mplus

-- what does this look like without inspecting Lift since that was the entire
-- point of implementing 'transform'?
instance (MonadPlus super, Monad super, Functor (Messages self))
    => MonadPlus (Synchronized a' a b' b self super)
  where

    mzero =
        Synchronized $ \_ _ -> super mzero

    mplus w0 w1 =
        Synchronized $ \up dn ->
            let
              routine =
                  runSynchronized w0 (unsafeCoerce up) (unsafeCoerce dn)
            in
              rewriteMplus up dn routine
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

type Effect self super r = Synchronized X () () X self super r

type Producer b self super r = Synchronized X () () b self super r

producer
    :: forall self super b r.
       ('[Sync] <: self, Monad super)
    => ((b -> Ef self super ()) -> Ef self super r)
    -> Producer' b self super r

producer f =
    Synchronized $ \_ dn ->
        do
          let
            scopedDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond :: Int -> b -> Ef self super ()
            respond scope b =
                Send (Respond scope b Return)

          i <- lift (getScope scopedDown)
          f (respond i)

type Consumer a self super r = Synchronized () a () X self super r

consumer
    :: forall self super a r.
       ('[Sync] <: self, Monad super)
    => (Ef self super a -> Ef self super r)
    -> Consumer' a self super r

consumer f =
    Synchronized $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request :: Int -> Ef self super a
            request scope =
                Send (Request scope () Return)

          i <- lift (getScope scopedUp)
          f (request i)

type Channel a b self super r = Synchronized () a () b self super r

channel
    :: forall self super a b r.
       ('[Sync] <: self, Monad super)
    => (Ef self super a -> (b -> Ef self super ()) -> Ef self super r)
    -> Channel a b self super r

channel f =
    Synchronized $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

          i <- lift (getScope scopedUp)
          let
            request =
                Send (Request i () Return)

            respond b =
                Send (Respond i b Return)

          f request respond

type Client a' a self super r = Synchronized a' a () X self super r

type Server b' b self super r = Synchronized X () b' b self super r

newtype Synchronized a' a b' b self super r =
    Synchronized
        {
          runSynchronized
              :: (forall x. a' -> (a -> Ef self super x) -> Ef self super x)
              -> (forall x. b -> (b' -> Ef self super x) -> Ef self super x)
              -> Ef self super r
        }

synchronized
    :: forall self a a' b b' super r.
       ('[Sync] <: self, Monad super)
    => ((a' -> Ef self super a) -> (b -> Ef self super b') -> Ef self super r)
    -> Synchronized a' a b' b self super r

synchronized f =
    Synchronized $ \up _ ->
        do
            let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
            i <- lift (getScope scopedUp)
            let request a = Send (Request i a Return)
                respond b' = Send (Respond i b' Return)
            f request respond

type Effect' self super r = forall x' x y' y. Synchronized x' x y' y self super r

type Producer' b self super r = forall x' x. Synchronized x' x () b self super r

type Consumer' a self super r = forall y' y. Synchronized () a y' y self super r

type Server' b' b self super r = forall x' x. Synchronized x' x b' b self super r

type Client' a' a self super r = forall y' y. Synchronized a' a y' y self super r

--------------------------------------------------------------------------------
-- Respond; substitute yields

cat :: ('[Sync] <: self, Monad super) => Channel a a self super r
cat = channel $ \awt yld -> forever (awt >>= yld)

(//>) :: ('[Sync] <: self, Monad super)
      => Synchronized x' x b' b self super a'
      -> (b -> Synchronized x' x c' c self super b')
      -> Synchronized x' x c' c self super a'
p0 //> fb =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        let routine = runSynchronized p0 up (unsafeCoerce dn)
        substituteResponds fb i up dn routine

substituteResponds
    :: forall self super x' x c' c b' b a'.
       ('[Sync] <: self, Monad super)
    => (b -> Synchronized x' x c' c self super b')
    -> Int
    -> (forall r. x' -> (x -> Ef self super r) -> Ef self super r)
    -> (forall r. c -> (c' -> Ef self super r) -> Ef self super r)
    -> Ef self super a'
    -> Ef self super a'
substituteResponds fb rewriteScope up dn =
    transform id go
  where

    go :: Messages self (Ef self super a') -> Ef self super a'
    go message =
        case prj message of
            Just (Respond currentScope b k) ->
                if currentScope == rewriteScope
                then do
                  res <- runSynchronized (fb (unsafeCoerce b)) (unsafeCoerce up) (unsafeCoerce dn)
                  transform id go (unsafeCoerce k res)
                else Do (fmap (transform id go) message)
            _ -> Do (fmap (transform id go) message)

for :: ('[Sync] <: self, Monad super)
    => Synchronized x' x b' b self super a'
    -> (b -> Synchronized x' x c' c self super b')
    -> Synchronized x' x c' c self super a'
for = (//>)

(<\\) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized x' x c' c self super b')
      -> Synchronized x' x b' b self super a'
      -> Synchronized x' x c' c self super a'
f <\\ p = p //> f

(\<\) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized x' x c' c self super b')
      -> (a -> Synchronized x' x b' b self super a')
      -> a
      -> Synchronized x' x c' c self super a'
p1 \<\ p2 = p2 />/ p1

(~>) :: ('[Sync] <: self, Monad super)
     => (a -> Synchronized x' x b' b self super a')
     -> (b -> Synchronized x' x c' c self super b')
     -> a
     -> Synchronized x' x c' c self super a'
(~>) = (/>/)

(<~) :: ('[Sync] <: self, Monad super)
     => (b -> Synchronized x' x c' c self super b')
     -> (a -> Synchronized x' x b' b self super a')
     -> a
     -> Synchronized x' x c' c self super a'
g <~ f = f ~> g

(/>/) :: ('[Sync] <: self, Monad super)
      => (a -> Synchronized x' x b' b self super a')
      -> (b -> Synchronized x' x c' c self super b')
      -> a
      -> Synchronized x' x c' c self super a'
(fa />/ fb) a = fa a //> fb

--------------------------------------------------------------------------------
-- Request; substitute awaits

(>\\) :: ('[Sync] <: self, Monad super)
      => (b' -> Synchronized a' a y' y self super b)
      -> Synchronized b' b y' y self super c
      -> Synchronized a' a y' y self super c
fb' >\\ p0 =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        let routine = runSynchronized p0 (unsafeCoerce up) dn
        substituteRequests fb' i up dn routine

substituteRequests
    :: forall self super x' x c' c b' b a'.
       ('[Sync] <: self, Monad super)
    => (b -> Synchronized x' x c' c self super b')
    -> Int
    -> (forall r. x' -> (x -> Ef self super r) -> Ef self super r)
    -> (forall r. c -> (c' -> Ef self super r) -> Ef self super r)
    -> Ef self super a'
    -> Ef self super a'
substituteRequests fb' rewriteScope up dn =
    transform id go
  where

    go :: Messages self (Ef self super a') -> Ef self super a'
    go message =
        case prj message of
            Just (Request currentScope b' k) -> do
                if currentScope == rewriteScope
                then do
                    res <- runSynchronized (fb' (unsafeCoerce b')) (unsafeCoerce up) (unsafeCoerce dn)
                    transform id go (unsafeCoerce k res)
                else Do (fmap (transform id go) message)
            _ -> Do (fmap (transform id go) message)



(/</) :: ('[Sync] <: self, Monad super)
      => (c' -> Synchronized b' b x' x self super c)
      -> (b' -> Synchronized a' a x' x self super b)
      -> c'
      -> Synchronized a' a x' x self super c
p1 /</ p2 = p2 \>\ p1

(>~) :: ('[Sync] <: self, Monad super)
     => Synchronized a' a y' y self super b
     -> Synchronized () b y' y self super c
     -> Synchronized a' a y' y self super c
p1 >~ p2 = (\() -> p1) >\\ p2

(~<) :: ('[Sync] <: self, Monad super)
     => Synchronized () b y' y self super c
     -> Synchronized a' a y' y self super b
     -> Synchronized a' a y' y self super c
p2 ~< p1 = p1 >~ p2

(\>\) :: ('[Sync] <: self, Monad super)
      => (b' -> Synchronized a' a y' y self super b)
      -> (c' -> Synchronized b' b y' y self super c)
      -> c'
      -> Synchronized a' a y' y self super c
(fb' \>\ fc') c' = fb' >\\ fc' c'

(//<) :: ('[Sync] <: self, Monad super)
      => Synchronized b' b y' y self super c
      -> (b' -> Synchronized a' a y' y self super b)
      -> Synchronized a' a y' y self super c
p //< f = f >\\ p

--------------------------------------------------------------------------------
-- Push; substitute responds with requests

(>>~)
    :: forall self a' a b' b c' c super r.
       (Monad super, '[Sync] <: self)
    => Synchronized a' a b' b self super r
    -> (b -> Synchronized b' b c' c self super r)
    -> Synchronized a' a c' c self super r
p0 >>~ fb0 =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        pushRewrite i up dn fb0 p0

pushRewrite
    :: forall self super r a' a b' b c' c.
       ('[Sync] <: self, Monad super)
    => Int
    -> (forall x. a' -> (a -> Ef self super x) -> Ef self super x)
    -> (forall x. c -> (c' -> Ef self super x) -> Ef self super x)
    -> (b -> Synchronized b' b c' c self super r)
    -> Synchronized a' a b' b self super r
    -> Ef self super r
pushRewrite rewriteScope up dn fb0 p0 =
    let upstream = runSynchronized p0 (unsafeCoerce up) (unsafeCoerce dn)
        downstream b = runSynchronized (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)
    in goLeft downstream upstream
  where
    goLeft fb =
      transform id goLeft'
      where
        goLeft' :: Messages self (Ef self super r) -> Ef self super r
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
        goRight' :: Messages self (Ef self super r) -> Ef self super r
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

(<~<) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized b' b c' c self super r)
      -> (a -> Synchronized a' a b' b self super r)
      -> a
      -> Synchronized a' a c' c self super r
p1 <~< p2 = p2 >~> p1

(>~>) :: ('[Sync] <: self, Monad super)
      => (_a -> Synchronized a' a b' b self super r)
      -> (b -> Synchronized b' b c' c self super r)
      -> _a
      -> Synchronized a' a c' c self super r
(fa >~> fb) a = fa a >>~ fb

(~<<) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized b' b c' c self super r)
      -> Synchronized a' a b' b self super r
      -> Synchronized a' a c' c self super r
k ~<< p = p >>~ k

--------------------------------------------------------------------------------
-- Pull; substitute requests with responds

(+>>) :: ('[Sync] <: self, Monad super)
      => (b' -> Synchronized a' a b' b self super r)
      ->        Synchronized b' b c' c self super r
      ->        Synchronized a' a c' c self super r
fb' +>> p0 =
    Synchronized $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        pullRewrite i up dn fb' p0

pullRewrite
    :: forall self super a' a b' b c' c r.
       ('[Sync] <: self, Monad super)
    => Int
    -> (forall x. a' -> (a -> Ef self super x) -> Ef self super x)
    -> (forall x. c -> (c' -> Ef self super x) -> Ef self super x)
    -> (b' -> Synchronized a' a b' b self super r)
    -> Synchronized b' b c' c self super r
    -> Ef self super r
pullRewrite rewriteScope up dn fb' p =
    let upstream b' = runSynchronized (fb' b') (unsafeCoerce up) (unsafeCoerce dn)
        downstream = runSynchronized p (unsafeCoerce up) (unsafeCoerce dn)
    in goRight upstream downstream
  where
    goRight fb'' =
      transform id goRight'
      where
        goRight' :: Messages self (Ef self super r) -> Ef self super r
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
        goLeft' :: Messages self (Ef self super r) -> Ef self super r
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

(>->) :: ('[Sync] <: self, Monad super)
      => Synchronized a' a () b self super r
      -> Synchronized () b c' c self super r
      -> Synchronized a' a c' c self super r
p1 >-> p2 = (\() -> p1) +>> p2

(<-<) :: ('[Sync] <: self, Monad super)
      => Synchronized () b c' c self super r
      -> Synchronized a' a () b self super r
      -> Synchronized a' a c' c self super r
p2 <-< p1 = p1 >-> p2

(<+<) :: ('[Sync] <: self, Monad super)
      => (c' -> Synchronized b' b c' c self super r)
      -> (b' -> Synchronized a' a b' b self super r)
      -> c'
      -> Synchronized a' a c' c self super r
p1 <+< p2 = p2 >+> p1

(>+>) :: ('[Sync] <: self, Monad super)
      => (b' -> Synchronized a' a b' b self super r)
      -> (_c' -> Synchronized b' b c' c self super r)
      -> _c'
      -> Synchronized a' a c' c self super r
(fb' >+> fc') c' = fb' +>> fc' c'

(<<+) :: ('[Sync] <: self, Monad super)
      => Synchronized b' b c' c self super r
      -> (b' -> Synchronized a' a b' b self super r)
      -> Synchronized a' a c' c self super r
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
