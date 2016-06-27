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

import Unsafe.Coerce

instance Ma Sync Sync where
    ma use (Sync i k) (FreshScope ik) = use k (ik i)

data Sync k where
    Sync :: Int -> k -> Sync k
    FreshScope :: (Int -> k) -> Sync k
    Request :: Int -> a' -> (a -> Narrative self super r) -> Sync k
    Respond :: Int -> b -> (b' -> Narrative self super r) -> Sync k

sync :: (Monad super, '[Sync] <. traits)
     => Trait Sync traits super
sync = Sync 0 $ \fs ->
    let Sync n k = view fs
        n' = succ n
    in n' `seq` pure $ fs .= Sync n' k
{-# INLINE sync #-}

freshScope :: (Monad super, '[Sync] <: self) => Narrative self super Int
freshScope = self (FreshScope id)

getScope :: ('[Sync] <: self, Monad super) => Narrative self super a -> super Int
getScope (Say symbol _) =
    case prj symbol of
        Just x ->
            case x of
                Request i _ _ -> return i
                Respond i _ _ -> return i

runSync :: ('[Sync] <: self, Monad super)
        => Effect self super r -> Narrative self super r
runSync e = do
    scope <- freshScope
    rewrite scope $
        runSynchronized e
            (\a' apl -> self (Request scope a' apl))
            (\b b'p -> self (Respond scope b b'p))

rewrite :: forall self super result.
           ('[Sync] <: self, Monad super)
        => Int -> Narrative self super result -> Narrative self super result
rewrite rewriteScope = transform go
  where

    go :: forall x. Messages self x -> (x -> Narrative self super result) -> Narrative self super result
    go message k =
        let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
            ignore = Say message (transform go . k)
        in case prj message of
               Just (Request currentScope a' _) -> check currentScope $ closed (unsafeCoerce a')
               Just (Respond currentScope b _) -> check currentScope $ closed (unsafeCoerce b)
               Nothing -> Say message (transform go . k)

instance Functor super
    => Functor (Synchronized a' a b' b self super)
  where

    fmap f (Synchronized w) =
        Synchronized $ \up dn -> fmap f (w up dn)

instance Monad super
    => Applicative (Synchronized a' a b' b self super)
  where

    pure a =
        Synchronized $ \_ _ -> pure a

    wf <*> wx =
        Synchronized $ \up dn -> do
            f <- runSynchronized wf up dn
            fmap f (runSynchronized wx (unsafeCoerce up) (unsafeCoerce dn))

    (*>) = (>>)

instance Monad super
    => Monad (Synchronized a' a b' b self super)
  where

    return = pure

    r >>= rs =
        Synchronized $ \up dn -> do
            v <- runSynchronized r (unsafeCoerce up) (unsafeCoerce dn)
            runSynchronized (rs v) up dn

instance ( Monad super
         , Monoid r
         ) => Monoid (Synchronized a' a b' b self super r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        Synchronized $ \up dn -> do
            result <- runSynchronized w1 up dn
            fmap (mappend result) $ runSynchronized w2 (unsafeCoerce up) (unsafeCoerce dn)

instance MonadPlus super
    => Alternative (Synchronized a' a b' b self super)
  where

    empty = mzero

    (<|>) = mplus

-- what does this look like without inspecting Super since that was the entire
-- point of implementing 'transform'?
instance MonadPlus super
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

            go (Fail err) =
                Fail err

            go (Return r) =
                Return r

            go (Say sym bp) =
                Say sym (go . bp)

            go (Super sup) =
              let
                routine =
                    runSynchronized w1 (unsafeCoerce up) (unsafeCoerce dn)

              in
                Super (fmap go sup `mplus` return routine)

newtype X = X X

closed :: X -> a
closed (X x) = closed x

type Effect self super r = Synchronized X () () X self super r

type Producer b self super r = Synchronized X () () b self super r

producer
    :: forall self super b r.
       ('[Sync] <: self, Monad super)
    => ((b -> Narrative self super ()) -> Narrative self super r)
    -> Producer' b self super r

producer f =
    Synchronized $ \_ dn ->
        do
          let
            scopedDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond :: Int -> b -> Narrative self super ()
            respond scope b =
                self (Respond scope b Return)

          i <- lift (getScope scopedDown)
          f (respond i)

type Consumer a self super r = Synchronized () a () X self super r

consumer
    :: forall self super a r.
       ('[Sync] <: self, Monad super)
    => (Narrative self super a -> Narrative self super r)
    -> Consumer' a self super r

consumer f =
    Synchronized $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request :: Int -> Narrative self super a
            request scope =
                self (Request scope () Return)

          i <- lift (getScope scopedUp)
          f (request i)

type Channel a b self super r = Synchronized () a () b self super r

channel
    :: forall self super a b r.
       ('[Sync] <: self, Monad super)
    => (Narrative self super a -> (b -> Narrative self super ()) -> Narrative self super r)
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
                self (Request i () Return)

            respond b =
                self (Respond i b Return)

          f request respond

type Client a' a self super r = Synchronized a' a () X self super r

type Server b' b self super r = Synchronized X () b' b self super r

newtype Synchronized a' a b' b self super r =
    Synchronized
        {
          runSynchronized
              :: (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
              -> (forall x. b -> (b' -> Narrative self super x) -> Narrative self super x)
              -> Narrative self super r
        }

synchronized
    :: forall self a a' b b' super r.
       ('[Sync] <: self, Monad super)
    => ((a' -> Narrative self super a) -> (b -> Narrative self super b') -> Narrative self super r)
    -> Synchronized a' a b' b self super r

synchronized f =
    Synchronized $ \up _ ->
        do
            let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
            i <- lift (getScope scopedUp)
            let request a = self (Request i a Return)
                respond b' = self (Respond i b' Return)
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

infixl 3 //>
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
    -> (forall r. x' -> (x -> Narrative self super r) -> Narrative self super r)
    -> (forall r. c -> (c' -> Narrative self super r) -> Narrative self super r)
    -> Narrative self super a'
    -> Narrative self super a'
substituteResponds fb rewriteScope up dn =
    transform go
  where

    go :: forall z. Messages self z -> (z -> Narrative self super a') -> Narrative self super a'
    go message k =
        let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
            ignore = Say message (transform go . k)
        in case prj message of

               Just x ->
                   case x of

                       Respond currentScope b _ ->
                           check currentScope $ do
                               let routine = runSynchronized
                                               (fb (unsafeCoerce b))
                                               (unsafeCoerce up)
                                               (unsafeCoerce dn)
                               res <- routine
                               let continue = k (unsafeCoerce res)
                               transform go continue
                       _ -> ignore
               _ -> ignore

for :: ('[Sync] <: self, Monad super)
    => Synchronized x' x b' b self super a'
    -> (b -> Synchronized x' x c' c self super b')
    -> Synchronized x' x c' c self super a'
for = (//>)

infixr 3 <\\
(<\\) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized x' x c' c self super b')
      -> Synchronized x' x b' b self super a'
      -> Synchronized x' x c' c self super a'
f <\\ p = p //> f

infixl 4 \<\
(\<\) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized x' x c' c self super b')
      -> (a -> Synchronized x' x b' b self super a')
      -> a
      -> Synchronized x' x c' c self super a'
p1 \<\ p2 = p2 />/ p1

infixr 4 ~>
(~>) :: ('[Sync] <: self, Monad super)
     => (a -> Synchronized x' x b' b self super a')
     -> (b -> Synchronized x' x c' c self super b')
     -> a
     -> Synchronized x' x c' c self super a'
(~>) = (/>/)

infixl 4 <~
(<~) :: ('[Sync] <: self, Monad super)
     => (b -> Synchronized x' x c' c self super b')
     -> (a -> Synchronized x' x b' b self super a')
     -> a
     -> Synchronized x' x c' c self super a'
g <~ f = f ~> g

infixr 4 />/
(/>/) :: ('[Sync] <: self, Monad super)
      => (a -> Synchronized x' x b' b self super a')
      -> (b -> Synchronized x' x c' c self super b')
      -> a
      -> Synchronized x' x c' c self super a'
(fa />/ fb) a = fa a //> fb

--------------------------------------------------------------------------------
-- Request; substitute awaits

infixr 4 >\\
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
    -> (forall r. x' -> (x -> Narrative self super r) -> Narrative self super r)
    -> (forall r. c -> (c' -> Narrative self super r) -> Narrative self super r)
    -> Narrative self super a'
    -> Narrative self super a'
substituteRequests fb' rewriteScope up dn =
    transform go
  where

    go :: forall z. Messages self z -> (z -> Narrative self super a') -> Narrative self super a'
    go message k =
        let
          check currentScope scoped =
              if currentScope == rewriteScope then
                  scoped
              else
                  ignore

          ignore =
              Say message (transform go . k)

        in
          case prj message of

              Just x ->
                  case x of

                      Request currentScope b' _ ->
                          check currentScope $ do
                              let routine = runSynchronized
                                              (fb' (unsafeCoerce b'))
                                              (unsafeCoerce up)
                                              (unsafeCoerce dn)
                              res <- routine
                              let continue = k (unsafeCoerce res)
                              transform go continue
                      _ -> ignore
              _ -> ignore

infixr 5 /</
(/</) :: ('[Sync] <: self, Monad super)
      => (c' -> Synchronized b' b x' x self super c)
      -> (b' -> Synchronized a' a x' x self super b)
      -> c'
      -> Synchronized a' a x' x self super c
p1 /</ p2 = p2 \>\ p1

infixr 5 >~
(>~) :: ('[Sync] <: self, Monad super)
     => Synchronized a' a y' y self super b
     -> Synchronized () b y' y self super c
     -> Synchronized a' a y' y self super c
p1 >~ p2 = (\() -> p1) >\\ p2

infixl 5 ~<
(~<) :: ('[Sync] <: self, Monad super)
     => Synchronized () b y' y self super c
     -> Synchronized a' a y' y self super b
     -> Synchronized a' a y' y self super c
p2 ~< p1 = p1 >~ p2

infixl 5 \>\
(\>\) :: ('[Sync] <: self, Monad super)
      => (b' -> Synchronized a' a y' y self super b)
      -> (c' -> Synchronized b' b y' y self super c)
      -> c'
      -> Synchronized a' a y' y self super c
(fb' \>\ fc') c' = fb' >\\ fc' c'

infixl 4 //<
(//<) :: ('[Sync] <: self, Monad super)
      => Synchronized b' b y' y self super c
      -> (b' -> Synchronized a' a y' y self super b)
      -> Synchronized a' a y' y self super c
p //< f = f >\\ p

--------------------------------------------------------------------------------
-- Push; substitute responds with requests

infixl 7 >>~
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
    -> (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b -> Synchronized b' b c' c self super r)
    -> Synchronized a' a b' b self super r
    -> Narrative self super r
pushRewrite rewriteScope up dn fb0 p0 =
    let upstream = runSynchronized p0 (unsafeCoerce up) (unsafeCoerce dn)
        downstream b = runSynchronized (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)
    in goLeft downstream upstream
  where

    goLeft fb =
        transform goLeft'
      where

        goLeft' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goLeft' message k =
            let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
                ignore = Say message (transform goLeft' . k)
            in case prj message of
                   Just x ->
                       case x of
                          Respond currentScope b _ ->
                            check currentScope $ goRight (unsafeCoerce k) (fb (unsafeCoerce b))
                          _ -> ignore
                   _ -> ignore

    goRight b'p =
        transform goRight'
      where

        goRight' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goRight' message k =
            let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
                ignore = Say message (transform goRight' . k)
            in case prj message of
                   Just x  ->
                       case x of
                           Request currentScope b' _ ->
                              check currentScope $ goLeft (unsafeCoerce k) (b'p (unsafeCoerce b'))
                           _ -> ignore
                   _ -> ignore

infixl 8 <~<
(<~<) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized b' b c' c self super r)
      -> (a -> Synchronized a' a b' b self super r)
      -> a
      -> Synchronized a' a c' c self super r
p1 <~< p2 = p2 >~> p1

infixr 8 >~>
(>~>) :: ('[Sync] <: self, Monad super)
      => (_a -> Synchronized a' a b' b self super r)
      -> (b -> Synchronized b' b c' c self super r)
      -> _a
      -> Synchronized a' a c' c self super r
(fa >~> fb) a = fa a >>~ fb

infixr 7 ~<<
(~<<) :: ('[Sync] <: self, Monad super)
      => (b -> Synchronized b' b c' c self super r)
      -> Synchronized a' a b' b self super r
      -> Synchronized a' a c' c self super r
k ~<< p = p >>~ k

--------------------------------------------------------------------------------
-- Pull; substitute requests with responds

infixr 6 +>>
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
    -> (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b' -> Synchronized a' a b' b self super r)
    -> Synchronized b' b c' c self super r
    -> Narrative self super r
pullRewrite rewriteScope up dn fb' p =
    let upstream b' = runSynchronized (fb' b') (unsafeCoerce up) (unsafeCoerce dn)
        downstream = runSynchronized p (unsafeCoerce up) (unsafeCoerce dn)
    in goRight upstream downstream
  where

    goRight fb'' =
        transform goRight'
      where

        goRight' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goRight' message k =
            let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
                ignore = Say message (transform goRight' . k)
            in case prj message of
                   Just x ->
                       case x of
                           Request currentScope b' _ ->
                               check currentScope $ goLeft (unsafeCoerce k) (fb'' (unsafeCoerce b'))
                           _ -> ignore
                   _ -> ignore

    goLeft bp =
        transform goLeft'
      where

        goLeft' :: forall x. Messages self x -> (x -> Narrative self super r) -> Narrative self super r
        goLeft' message k' =
            let check currentScope scoped = if currentScope == rewriteScope then scoped else ignore
                ignore = Say message (transform goLeft' . k')
            in case prj message of
                   Just x ->
                       case x of
                           Respond currentScope b _ ->
                               check currentScope $ goRight (unsafeCoerce k') (bp (unsafeCoerce b))
                           _ -> ignore
                   _ -> ignore

infixl 7 >->
(>->) :: ('[Sync] <: self, Monad super)
      => Synchronized a' a () b self super r
      -> Synchronized () b c' c self super r
      -> Synchronized a' a c' c self super r
p1 >-> p2 = (\() -> p1) +>> p2

infixr 7 <-<
(<-<) :: ('[Sync] <: self, Monad super)
      => Synchronized () b c' c self super r
      -> Synchronized a' a () b self super r
      -> Synchronized a' a c' c self super r
p2 <-< p1 = p1 >-> p2

infixr 7 <+<
(<+<) :: ('[Sync] <: self, Monad super)
      => (c' -> Synchronized b' b c' c self super r)
      -> (b' -> Synchronized a' a b' b self super r)
      -> c'
      -> Synchronized a' a c' c self super r
p1 <+< p2 = p2 >+> p1

infixl 7 >+>
(>+>) :: ('[Sync] <: self, Monad super)
      => (b' -> Synchronized a' a b' b self super r)
      -> (_c' -> Synchronized b' b c' c self super r)
      -> _c'
      -> Synchronized a' a c' c self super r
(fb' >+> fc') c' = fb' +>> fc' c'

infixl 6 <<+
(<<+) :: ('[Sync] <: self, Monad super)
      => Synchronized b' b c' c self super r
      -> (b' -> Synchronized a' a b' b self super r)
      -> Synchronized a' a c' c self super r
p <<+ fb = fb +>> p

-- valid rules from pipes
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

{-# RULES
    "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

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
