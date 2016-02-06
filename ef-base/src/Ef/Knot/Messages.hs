{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ef.Knot.Messages
    ( Knot(..)
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



import Ef.Narrative

import Control.Applicative
import Control.Monad
import Unsafe.Coerce

import Ef.Messages


data Knot k where
    FreshScope :: (Int -> k) -> Knot k
    Request :: Int -> a' -> (a -> Narrative self super r) -> Knot k
    Respond :: Int -> b -> (b' -> Narrative self super r) -> Knot k


freshScope :: Invoke Knot self super Int
freshScope = self (FreshScope id)


getScope :: (Can Knot self, Monad super) => Narrative self super a -> super Int
getScope (Say symbol _) =
    case prj symbol of
        Just x ->
            case x of
                Request i _ _ -> return i
                Respond i _ _ -> return i


linearize :: (Can Knot self, Monad super)
          => Effect self super r -> Invoke Knot self super r
linearize e = do
    scope <- freshScope
    rewrite scope $
        runKnotted e
            (\a' apl -> self (Request scope a' apl))
            (\b b'p -> self (Respond scope b b'p))


rewrite :: forall self super result.
           (Can Knot self, Monad super)
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
    => Functor (Knotted a' a b' b self super)
  where

    fmap f (Knotted w) =
        Knotted $ \up dn -> fmap f (w up dn)


instance Monad super
    => Applicative (Knotted a' a b' b self super)
  where

    pure a =
        Knotted $ \_ _ -> pure a

    wf <*> wx =
        Knotted $ \up dn -> do
            f <- runKnotted wf up dn
            fmap f (runKnotted wx (unsafeCoerce up) (unsafeCoerce dn))

    (*>) = (>>)


instance Monad super
    => Monad (Knotted a' a b' b self super)
  where

    return = pure

    r >>= rs =
        Knotted $ \up dn -> do
            v <- runKnotted r (unsafeCoerce up) (unsafeCoerce dn)
            runKnotted (rs v) up dn


instance ( Monad super
         , Monoid r
         ) => Monoid (Knotted a' a b' b self super r)
  where

    mempty =
        pure mempty

    mappend w1 w2 =
        Knotted $ \up dn -> do
            result <- runKnotted w1 up dn
            fmap (mappend result) $ runKnotted w2 (unsafeCoerce up) (unsafeCoerce dn)



instance MonadPlus super
    => Alternative (Knotted a' a b' b self super)
  where

    empty = mzero

    (<|>) = mplus


-- what does this look like without inspecting Super since that was the entire
-- point of implementing 'transform'?
instance MonadPlus super
    => MonadPlus (Knotted a' a b' b self super)
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


closed :: X -> a
closed (X x) = closed x


type Effect self super r = Knotted X () () X self super r

type Producer b self super r = Knotted X () () b self super r


producer
    :: forall self super b r.
       (Can Knot self, Monad super)
    => ((b -> Narrative self super ()) -> Narrative self super r)
    -> Producer' b self super r

producer f =
    Knotted $ \_ dn ->
        do
          let
            scopedDown =
                dn (unsafeCoerce ()) (unsafeCoerce ())

            respond :: Int -> b -> Narrative self super ()
            respond scope b =
                self (Respond scope b Return)

          i <- lift (getScope scopedDown)
          f (respond i)


type Consumer a self super r = Knotted () a () X self super r


consumer
    :: forall self super a r.
       (Can Knot self, Monad super)
    => (Narrative self super a -> Narrative self super r)
    -> Consumer' a self super r

consumer f =
    Knotted $ \up _ ->
        do
          let
            scopedUp =
                up (unsafeCoerce ()) (unsafeCoerce ())

            request :: Int -> Narrative self super a
            request scope =
                self (Request scope () Return)

          i <- lift (getScope scopedUp)
          f (request i)


type Line a b self super r = Knotted () a () b self super r


line
    :: forall self super a b r.
       (Can Knot self, Monad super)
    => (Narrative self super a -> (b -> Narrative self super ()) -> Narrative self super r)
    -> Line a b self super r

line f =
    Knotted $ \up _ ->
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


type Client a' a self super r = Knotted a' a () X self super r


type Server b' b self super r = Knotted X () b' b self super r


newtype Knotted a' a b' b self super r =
    Knotted
        {
          runKnotted
              :: (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
              -> (forall x. b -> (b' -> Narrative self super x) -> Narrative self super x)
             -> Narrative self super r
        }



knotted
    :: forall self a a' b b' super r.
       (Can Knot self, Monad super)
    => ((a' -> Narrative self super a) -> (b -> Narrative self super b') -> Narrative self super r)
    -> Knotted a' a b' b self super r

knotted f =
    Knotted $ \up _ ->
        do
            let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
            i <- lift (getScope scopedUp)
            let request a = self (Request i a Return)
                respond b' = self (Respond i b' Return)
            f request respond

type Effect' self super r = forall x' x y' y. Knotted x' x y' y self super r

type Producer' b self super r = forall x' x. Knotted x' x () b self super r

type Consumer' a self super r = forall y' y. Knotted () a y' y self super r

type Server' b' b self super r = forall x' x. Knotted x' x b' b self super r

type Client' a' a self super r = forall y' y. Knotted a' a y' y self super r

--------------------------------------------------------------------------------
-- Respond; substitute yields

cat :: (Can Knot self, Monad super) => Line a a self super r
cat = line $ \awt yld -> forever (awt >>= yld)


infixl 3 //>
(//>) :: (Can Knot self, Monad super)
      => Knotted x' x b' b self super a'
      -> (b -> Knotted x' x c' c self super b')
      -> Knotted x' x c' c self super a'
p0 //> fb =
    Knotted $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        let routine = runKnotted p0 up (unsafeCoerce dn)
        substituteResponds fb i up dn routine


substituteResponds
    :: forall self super x' x c' c b' b a'.
       (Can Knot self, Monad super)
    => (b -> Knotted x' x c' c self super b')
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
                               let routine = runKnotted (fb (unsafeCoerce b))
                                                        (unsafeCoerce up)
                                                        (unsafeCoerce dn)
                               res <- routine
                               let continue = k (unsafeCoerce res)
                               transform go continue
                       _ -> ignore
               _ -> ignore


for :: (Can Knot self, Monad super)
    => Knotted x' x b' b self super a'
    -> (b -> Knotted x' x c' c self super b')
    -> Knotted x' x c' c self super a'
for = (//>)


infixr 3 <\\
(<\\) :: (Can Knot self, Monad super)
      => (b -> Knotted x' x c' c self super b')
      -> Knotted x' x b' b self super a'
      -> Knotted x' x c' c self super a'
f <\\ p = p //> f


infixl 4 \<\
(\<\) :: (Can Knot self, Monad super)
      => (b -> Knotted x' x c' c self super b')
      -> (a -> Knotted x' x b' b self super a')
      -> a
      -> Knotted x' x c' c self super a'
p1 \<\ p2 = p2 />/ p1


infixr 4 ~>
(~>) :: (Can Knot self, Monad super)
     => (a -> Knotted x' x b' b self super a')
     -> (b -> Knotted x' x c' c self super b')
     -> a
     -> Knotted x' x c' c self super a'
(~>) = (/>/)


infixl 4 <~
(<~) :: (Can Knot self, Monad super)
     => (b -> Knotted x' x c' c self super b')
     -> (a -> Knotted x' x b' b self super a')
     -> a
     -> Knotted x' x c' c self super a'
g <~ f = f ~> g


infixr 4 />/
(/>/) :: (Can Knot self, Monad super)
      => (a -> Knotted x' x b' b self super a')
      -> (b -> Knotted x' x c' c self super b')
      -> a
      -> Knotted x' x c' c self super a'
(fa />/ fb) a = fa a //> fb


--------------------------------------------------------------------------------
-- Request; substitute awaits


infixr 4 >\\
(>\\) :: (Can Knot self, Monad super)
      => (b' -> Knotted a' a y' y self super b)
      -> Knotted b' b y' y self super c
      -> Knotted a' a y' y self super c
fb' >\\ p0 =
    Knotted $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        let routine = runKnotted p0 (unsafeCoerce up) dn
        substituteRequests fb' i up dn routine


substituteRequests
    :: forall self super x' x c' c b' b a'.
       (Can Knot self, Monad super)
    => (b -> Knotted x' x c' c self super b')
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
                              let routine = runKnotted (fb' (unsafeCoerce b'))
                                                       (unsafeCoerce up)
                                                       (unsafeCoerce dn)
                              res <- routine
                              let continue = k (unsafeCoerce res)
                              transform go continue
                      _ -> ignore
              _ -> ignore


infixr 5 /</
(/</) :: (Can Knot self, Monad super)
      => (c' -> Knotted b' b x' x self super c)
      -> (b' -> Knotted a' a x' x self super b)
      -> c'
      -> Knotted a' a x' x self super c
p1 /</ p2 = p2 \>\ p1


infixr 5 >~
(>~) :: (Can Knot self, Monad super)
     => Knotted a' a y' y self super b
     -> Knotted () b y' y self super c
     -> Knotted a' a y' y self super c
p1 >~ p2 = (\() -> p1) >\\ p2


infixl 5 ~<
(~<) :: (Can Knot self, Monad super)
     => Knotted () b y' y self super c
     -> Knotted a' a y' y self super b
     -> Knotted a' a y' y self super c
p2 ~< p1 = p1 >~ p2


infixl 5 \>\
(\>\) :: (Can Knot self, Monad super)
      => (b' -> Knotted a' a y' y self super b)
      -> (c' -> Knotted b' b y' y self super c)
      -> c'
      -> Knotted a' a y' y self super c
(fb' \>\ fc') c' = fb' >\\ fc' c'


infixl 4 \\<
(\\<) :: (Can Knot self, Monad super)
      => Knotted b' b y' y self super c
      -> (b' -> Knotted a' a y' y self super b)
      -> Knotted a' a y' y self super c
p \\< f = f >\\ p


--------------------------------------------------------------------------------
-- Push; substitute responds with requests


infixl 7 >>~
(>>~)
    :: forall self a' a b' b c' c super r.
       (Can Knot self, Monad super)
    => Knotted a' a b' b self super r
    -> (b -> Knotted b' b c' c self super r)
    -> Knotted a' a c' c self super r
p0 >>~ fb0 =
    Knotted $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        pushRewrite i up dn fb0 p0


pushRewrite
    :: forall self super r a' a b' b c' c.
       (Can Knot self, Monad super)
    => Int
    -> (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b -> Knotted b' b c' c self super r)
    -> Knotted a' a b' b self super r
    -> Narrative self super r
pushRewrite rewriteScope up dn fb0 p0 =
    let upstream = runKnotted p0 (unsafeCoerce up) (unsafeCoerce dn)
        downstream b = runKnotted (fb0 b) (unsafeCoerce up) (unsafeCoerce dn)
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
(<~<) :: (Can Knot self, Monad super)
      => (b -> Knotted b' b c' c self super r)
      -> (a -> Knotted a' a b' b self super r)
      -> a
      -> Knotted a' a c' c self super r
p1 <~< p2 = p2 >~> p1


infixr 8 >~>
(>~>) :: (Can Knot self, Monad super)
      => (_a -> Knotted a' a b' b self super r)
      -> (b -> Knotted b' b c' c self super r)
      -> _a
      -> Knotted a' a c' c self super r
(fa >~> fb) a = fa a >>~ fb


infixr 7 ~<<
(~<<) :: (Can Knot self, Monad super)
      => (b -> Knotted b' b c' c self super r)
      -> Knotted a' a b' b self super r
      -> Knotted a' a c' c self super r
k ~<< p = p >>~ k


--------------------------------------------------------------------------------
-- Pull; substitute requests with responds


infixr 6 +>>
(+>>) :: (Can Knot self, Monad super)
      => (b' -> Knotted a' a b' b self super r)
      ->        Knotted b' b c' c self super r
      ->        Knotted a' a c' c self super r
fb' +>> p0 =
    Knotted $ \up dn -> do
        let scopedUp = up (unsafeCoerce ()) (unsafeCoerce ())
        i <- lift (getScope scopedUp)
        pullRewrite i up dn fb' p0


pullRewrite
    :: forall self super a' a b' b c' c r.
       (Can Knot self, Monad super)
    => Int
    -> (forall x. a' -> (a -> Narrative self super x) -> Narrative self super x)
    -> (forall x. c -> (c' -> Narrative self super x) -> Narrative self super x)
    -> (b' -> Knotted a' a b' b self super r)
    -> Knotted b' b c' c self super r
    -> Narrative self super r
pullRewrite rewriteScope up dn fb' p =
    let upstream b' = runKnotted (fb' b') (unsafeCoerce up) (unsafeCoerce dn)
        downstream = runKnotted p (unsafeCoerce up) (unsafeCoerce dn)
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
(>->) :: (Can Knot self, Monad super)
      => Knotted a' a () b self super r
      -> Knotted () b c' c self super r
      -> Knotted a' a c' c self super r
p1 >-> p2 = (\() -> p1) +>> p2


infixr 7 <-<
(<-<) :: (Can Knot self, Monad super)
      => Knotted () b c' c self super r
      -> Knotted a' a () b self super r
      -> Knotted a' a c' c self super r
p2 <-< p1 = p1 >-> p2


infixr 7 <+<
(<+<) :: (Can Knot self, Monad super)
      => (c' -> Knotted b' b c' c self super r)
      -> (b' -> Knotted a' a b' b self super r)
      -> c'
      -> Knotted a' a c' c self super r
p1 <+< p2 = p2 >+> p1


infixl 7 >+>
(>+>) :: (Can Knot self, Monad super)
      => (b' -> Knotted a' a b' b self super r)
      -> (_c' -> Knotted b' b c' c self super r)
      -> _c'
      -> Knotted a' a c' c self super r
(fb' >+> fc') c' = fb' +>> fc' c'


infixl 6 <<+
(<<+) :: (Can Knot self, Monad super)
      => Knotted b' b c' c self super r
      -> (b' -> Knotted a' a b' b self super r)
      -> Knotted a' a c' c self super r
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
