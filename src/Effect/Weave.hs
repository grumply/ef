{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Weave
  ( Weaving, weaving
  , Weave, linearize
  , producer, Producer, Producer'
  , consumer, Consumer, Consumer'
  , pipe, Pipe
  , client, Client, Client'
  , server, Server, Server'
  , weave, Woven
  , fold, unfold
  , for, cat
  , X
  , (<\\), (\<\), (~>),  (<~) , (/>/), (//>)
  , (\\<), (/</), (>~),  (~<) , (\>\), (>\\)
  ,        (<~<), (~<<), (>>~), (>~>)
  , (<<+), (<+<), (<-<), (>->), (>+>), (+>>)
  ,       (<==<), (==<), (>==), (>==>)
  ,        (<--), (>--), (--<), (-->)
  ) where

import Mop hiding (push,pull)
import Control.Monad
import Data.Function
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

-- linearization of communicating threads

cat :: Has Weave fs m => Pipe fs a a m r
cat = pipe $ \await yield -> forever (await >>= yield)

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

data Weave k
  = FreshScope (Integer -> k)
  | forall fs a' a m r. Request Integer a' (a  -> PlanT fs m r)
  | forall fs b' b m r. Respond Integer b  (b' -> PlanT fs m r)

data Weaving k = Weaving (IORef Integer) k

{-# INLINABLE freshScope #-}
freshScope :: Has Weave fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

{-# INLINE weaving #-}
weaving :: Uses Weaving fs m => Instruction Weaving fs m
weaving = Weaving (unsafePerformIO (newIORef 0)) $ \fs ->
  let Weaving n k = view fs
      n' = unsafePerformIO (modifyIORef n succ)
  in n' `seq` return fs

getScope p =
  case p (unsafeCoerce ()) (unsafeCoerce ()) of
    Step sym bp ->
      case prj sym of
        Just x ->
          case x of
            Request i _ _ -> i
            Respond i _ _ -> i
            _ -> error "getScope got FreshScope"
        _ -> error "getScope got non-Weave"
    _ -> error "getScope got non-step"

instance Pair Weaving Weave where
  pair p (Weaving i k) (FreshScope ik) =
    let n = unsafePerformIO (readIORef i)
    in n `seq` p k (ik n)
  pair _ _ _ = error "Pipe primitive escaped its scope:\n\
                     \\tAttempting to reuse control flow\
                     \ primitives outside of their scope\
                     \ is unsupported."

{-# INLINE linearize #-}
linearize :: Has Weave fs m => Effect fs m r -> PlanT fs m r
linearize e = do
    scope <- freshScope
    go' scope $ e (\a' ap -> symbol (Request scope a' ap)) (\b b'p -> symbol (Respond scope b b'p))
  where
    go' scope p0 = go p0
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Request i x _ ->
                      if i == scope
                      then closed (unsafeCoerce x)
                      else Step sym (\b -> go (bp b))
                    Respond i x _ ->
                      if i == scope
                      then closed (unsafeCoerce x)
                      else Step sym (\b -> go (bp b))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

newtype X = X X

closed :: X -> a
closed (X x) = closed x

type Effect fs m r = Woven fs X () () X m r

type Producer fs b m r = Woven fs X () () b m r
-- producer $ \yield -> do { .. ; }
producer :: Has Weave fs m => ((b -> PlanT fs m ()) -> PlanT fs m r) -> Producer' fs b m r
producer f = \_ dn -> f (\b -> symbol (Respond (getScope dn) b Pure))

type Consumer fs a m r = Woven fs () a () X m r
-- consumer $ \await -> do { .. ; }
consumer :: Has Weave fs m => (PlanT fs m a -> PlanT fs m r) -> Consumer' fs a m r
consumer f = \up _ -> f (symbol (Request (getScope up) () Pure))

type Pipe fs a b m r = Woven fs () a () b m r
-- pipe $ \await yield -> do { .. ; }
pipe :: Has Weave fs m => (PlanT fs m a -> (b -> PlanT fs m x) -> PlanT fs m r) -> Pipe fs a b m r
pipe f = \up dn -> f (symbol (Request (getScope up) () Pure))
                     (\b -> symbol (Respond (getScope dn) b Pure))

type Client fs a' a m r = Woven fs a' a () X m r
-- client $ \request -> do { .. ; }
client :: Has Weave fs m => ((a -> PlanT fs m a') -> PlanT fs m r) -> Client' fs a' a m r
client f = \up _ -> f (\a -> symbol (Request (getScope up) a Pure))

type Server fs b' b m r = Woven fs X () b' b m r
-- server $ \respond -> do { .. ; }
server :: Has Weave fs m => ((b' -> PlanT fs m b) -> PlanT fs m r) -> Server' fs b' b m r
server f = \_ dn -> f (\b' -> symbol (Respond (getScope dn) b' Pure))

type Woven fs a' a b' b m r
  =  (forall x. a' -> (a -> PlanT fs m x) -> PlanT fs m x)
  -> (forall x. b -> (b' -> PlanT fs m x) -> PlanT fs m x)
  -> PlanT fs m r
-- weave $ \request respond -> do { .. ; }
weave :: Has Weave fs m => ((a -> PlanT fs m a') -> (b' -> PlanT fs m b) -> PlanT fs m r) -> Woven fs a' a b' b m r
weave f = \up dn -> f (\a -> symbol (Request (getScope up) a Pure))
                      (\b' -> symbol (Respond (getScope dn) b' Pure))

type Effect' fs m r = forall x' x y' y . Woven fs x' x y' y m r

type Producer' fs b m r = forall x' x . Woven fs x' x () b m r

type Consumer' fs a m r = forall y' y . Woven fs () a y' y m r

type Server' fs b' b m r = forall x' x . Woven fs x' x b' b m r

type Client' fs a' a m r = forall y' y . Woven fs a' a y' y m r

--------------------------------------------------------------------------------
-- the side the '>' or '<' is on determines where execution start. The greater
-- side says which side is eagerly evaluated. Thus, '>--' says: start evaluation
-- on the left and always execute as little as possible on the right. That means
-- we start evaluation on the left and find the first instance of a Respond in
-- our scope, we then switch sides to evaluate the right side and look for a
-- request. Since we have a value to fulfill a request, we have a choice:
-- fulfill the request and continue execution on the right or
-- fulfill the request and switch back to the left. The (>--) method prefers
-- switching back to the left side. (<--) prefers staying on the right as
-- long as possible. These each have uses, I believe. For instance, `-->`
-- is useful when you want allocation of a resource to happen as late as
-- possible on the left and cleanup needs to be done as soon as possible
-- when triggered by the right.

-- start execution on the left, execute as little as possible on the right
infixl 6 >--
(>--) :: forall fs a' a b' b c' c m r. Has Weave fs m
      => Woven fs a' a b' b m r
      -> Woven fs b' b c' c m r
      -> Woven fs a' a c' c m r
pl >-- pr = \up dn -> transform (getScope up) (pl up (unsafeCoerce dn))
                                              (pr (unsafeCoerce up) dn)
  where
    transform scope pl0 pr0 = start pr0 pl0
      where
        start pr = go
          where
            go pl =
              case pl of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i b _ ->
                          if i == scope
                          then goRight (unsafeCoerce bp) (unsafeCoerce b) pr
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        goRight bpl b = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i b' _ ->
                          if i == scope
                          then start (bp (unsafeCoerce b)) (bpl (unsafeCoerce b'))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

-- start execution on the left, execute as much as possible on the right
infixl 5 <--
(<--) :: forall fs a' a b' b c' c m r. Has Weave fs m
      => Woven fs a' a b' b m r
      -> Woven fs b' b c' c m r
      -> Woven fs a' a c' c m r
pl <-- pr = \up dn -> transform (getScope up) (pl up (unsafeCoerce dn))
                                              (pr (unsafeCoerce up) dn)
  where
    transform scope pl0 pr0 = start pr0 pl0
      where
        start pr = go
          where
            go pl =
              case pl of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i b _ ->
                          if i == scope
                          then goRight (unsafeCoerce bp) (unsafeCoerce b) pr
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        goRight bpl b = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i b' _ ->
                          if i == scope
                          then continueRight (bp (unsafeCoerce b)) (bpl (unsafeCoerce b'))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        continueRight l = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i _ _ ->
                          if i == scope
                          then start p l
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

-- start execution on the right, execute as little as possible on the left
infixr 6 --<
(--<) :: Has Weave fs m
      => Woven fs a' a b' b m r
      -> Woven fs b' b c' c m r
      -> Woven fs a' a c' c m r
pl --< pr = \up dn -> transform (getScope up) (pl up (unsafeCoerce dn))
                                              (pr (unsafeCoerce up) dn)
  where
    transform scope pl0 pr0 = start pl0 pr0
      where
        start pl = go
          where
            go pr =
              case pr of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i b _ ->
                          if i == scope
                          then goLeft (unsafeCoerce bp) (unsafeCoerce b) pl
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        goLeft bpr b = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i b' _ ->
                          if i == scope
                          then start (bp (unsafeCoerce b)) (bpr (unsafeCoerce b'))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

-- start execution on the right, execute as much as possible on the left
infixr 5 -->
(-->) :: Has Weave fs m
      => Woven fs a' a b' b m r
      -> Woven fs b' b c' c m r
      -> Woven fs a' a c' c m r
pl --> pr = \up dn -> transform (getScope up) (pl up (unsafeCoerce dn))
                                              (pr (unsafeCoerce up) dn)
  where
    transform scope pl0 pr0 = start pl0 pr0
      where
        start pl = go
          where
            go pr =
              case pr of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i b _ ->
                          if i == scope
                          then goLeft (unsafeCoerce bp) (unsafeCoerce b) pl
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        goLeft bpr b = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i b' _ ->
                          if i == scope
                          then continueLeft (bp (unsafeCoerce b)) (bpr (unsafeCoerce b'))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        continueLeft l r = go l
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i b' _ ->
                          if i == scope
                          then start p r
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

--------------------------------------------------------------------------------
-- Respond

for :: Has Weave fs m
    =>       Woven fs x' x b' b m a'
    -> (b -> Woven fs x' x c' c m b')
    ->       Woven fs x' x c' c m a'
for = (//>)

infixr 3 <\\
(<\\) :: Has Weave fs m
      => (b -> Woven fs x' x c' c m b')
      ->       Woven fs x' x b' b m a'
      ->       Woven fs x' x c' c m a'
f <\\ p = p //> f

infixl 4 \<\ --
(\<\) :: Has Weave fs m
      => (b -> Woven fs x' x c' c m b')
      -> (a -> Woven fs x' x b' b m a')
      ->  a -> Woven fs x' x c' c m a'
p1 \<\ p2 = p2 />/ p1

infixr 4 ~>
(~>) :: Has Weave fs m
     => (a -> Woven fs x' x b' b m a')
     -> (b -> Woven fs x' x c' c m b')
     ->  a -> Woven fs x' x c' c m a'
(~>) = (/>/)

infixl 4 <~
(<~) :: Has Weave fs m
     => (b -> Woven fs x' x c' c m b')
     -> (a -> Woven fs x' x b' b m a')
     ->  a -> Woven fs x' x c' c m a'
g <~ f = f ~> g

infixr 4 />/
(/>/) :: Has Weave fs m
      => (a -> Woven fs x' x b' b m a')
      -> (b -> Woven fs x' x c' c m b')
      ->  a -> Woven fs x' x c' c m a'
(fa />/ fb) a = fa a //> fb

infixl 3 //>
(//>) :: forall fs x' x b' b c' c m a'. Has Weave fs m
      =>       Woven fs x' x b' b m a'
      -> (b -> Woven fs x' x c' c m b')
      ->       Woven fs x' x c' c m a'
p0 //> fb = \up dn -> transform (getScope up) up dn (p0 up (unsafeCoerce dn))
  where
    transform scope up dn p0 = go p0
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Respond i b _ ->
                      if i == scope
                      then do
                        fb (unsafeCoerce b) (unsafeCoerce up) (unsafeCoerce dn)
                          >>= go . bp . unsafeCoerce
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

unfold :: ((b -> PlanT fs m b') -> PlanT fs m r) -> Woven fs a' a b' b m r
unfold u = \_ respond -> u (\x -> respond x Pure)

--------------------------------------------------------------------------------
-- Request


infixr 5 /</
(/</) :: Has Weave fs m
      => (c' -> Woven fs b' b x' x m c)
      -> (b' -> Woven fs a' a x' x m b)
      ->  c' -> Woven fs a' a x' x m c
p1 /</ p2 = p2 \>\ p1

infixr 5 >~
(>~) :: Has Weave fs m
     => Woven fs a' a y' y m b
     -> Woven fs () b y' y m c
     -> Woven fs a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2

infixl 5 ~<
(~<) :: Has Weave fs m
     => Woven fs () b y' y m c
     -> Woven fs a' a y' y m b
     -> Woven fs a' a y' y m c
p2 ~< p1 = p1 >~ p2

infixl 5 \>\ --
(\>\) :: Has Weave fs m
      => (b' -> Woven fs a' a y' y m b)
      -> (c' -> Woven fs b' b y' y m c)
      ->  c' -> Woven fs a' a y' y m c
(fb' \>\ fc') c' = fb' >\\ fc' c'

infixl 4 \\<
(\\<) :: forall fs y' y a' a b' b m c. Has Weave fs m
      =>        Woven fs b' b y' y m c
      -> (b' -> Woven fs a' a y' y m b)
      ->        Woven fs a' a y' y m c
p \\< f = f >\\ p

infixr 4 >\\ --
(>\\) :: forall fs y' y a' a b' b m c. Has Weave fs m
      => (b' -> Woven fs a' a y' y m b)
      ->        Woven fs b' b y' y m c
      ->        Woven fs a' a y' y m c
fb' >\\ p0 = \up dn -> transform (getScope up) up dn (p0 (unsafeCoerce up) dn)
  where
    transform scope up dn p1 = go p1
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    Request i b' _ ->
                      if i == scope
                      then do
                        fb' (unsafeCoerce b') (unsafeCoerce up) (unsafeCoerce dn)
                          >>= go . bp . unsafeCoerce
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r

fold :: ((a' -> PlanT fs m a) -> PlanT fs m r) -> Woven fs a' a b' b m r
fold f = \request _ -> f (\x -> request x Pure)

--------------------------------------------------------------------------------
-- Push

infixl 8 <~<
(<~<) :: Has Weave fs m
      => (b -> Woven fs b' b c' c m r)
      -> (a -> Woven fs a' a b' b m r)
      ->  a -> Woven fs a' a c' c m r
p1 <~< p2 = p2 >~> p1

infixr 8 >~>
(>~>) :: Has Weave fs m
      => (_a -> Woven fs a' a b' b m r)
      -> ( b -> Woven fs b' b c' c m r)
      ->  _a -> Woven fs a' a c' c m r
(fa >~> fb) a = fa a >>~ fb

infixr 7 ~<<
(~<<) :: Has Weave fs m
      => (b -> Woven fs b' b c' c m r)
      ->       Woven fs a' a b' b m r
      ->       Woven fs a' a c' c m r
k ~<< p = p >>~ k

infixl 7 >>~
(>>~) :: forall fs a' a b' b c' c m r. Has Weave fs m
      =>       Woven fs a' a b' b m r
      -> (b -> Woven fs b' b c' c m r)
      ->       Woven fs a' a c' c m r
p0 >>~ fb0 = \up dn -> transform (getScope up) up dn (p0 up (unsafeCoerce dn))
  where
    transform scope up dn p1 = go p1
      where
        go = goLeft (\b -> fb0 b (unsafeCoerce up) (unsafeCoerce dn))
          where
            goLeft :: (b -> PlanT fs m r) -> PlanT fs m r -> PlanT fs m r
            goLeft fb = goLeft'
              where
                goLeft' p =
                  case p of
                    Step sym bp ->
                      case prj sym of
                        Just x ->
                          case x of
                            Respond i b _ ->
                              if i == scope
                              then goRight (unsafeCoerce bp) (fb (unsafeCoerce b))
                              else Step sym (\b -> goLeft' (bp b))
                            _ -> Step sym (\b -> goLeft' (bp b))
                        Nothing -> Step sym (\b -> goLeft' (bp b))
                    M m -> M (fmap goLeft' m)
                    Pure r -> Pure r
            goRight :: (b' -> PlanT fs m r) -> PlanT fs m r -> PlanT fs m r
            goRight b'p = goRight'
              where
                goRight' p =
                  case p of
                    Step sym bp ->
                      case prj sym of
                        Just x ->
                          case x of
                            Request i b' _ ->
                              if i == scope
                              then goLeft (unsafeCoerce bp) (b'p (unsafeCoerce b'))
                              else Step sym (\b -> goRight' (bp b))
                            _ -> Step sym (\b -> goRight' (bp b))
                        Nothing -> Step sym (\b -> goRight' (bp b))
                    M m -> M (fmap goRight' m)
                    Pure r -> Pure r

--------------------------------------------------------------------------------
-- Pull

infixl 7 >->
(>->) :: Has Weave fs m
      => Woven fs a' a () b m r
      -> Woven fs () b c' c m r
      -> Woven fs a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2

infixr 7 <-<
(<-<) :: Has Weave fs m
      => Woven fs () b c' c m r
      -> Woven fs a' a () b m r
      -> Woven fs a' a c' c m r
p2 <-< p1 = p1 >-> p2

infixr 7 <+<
(<+<) :: Has Weave fs m
      => (c' -> Woven fs b' b c' c m r)
      -> (b' -> Woven fs a' a b' b m r)
      ->  c' -> Woven fs a' a c' c m r
p1 <+< p2 = p2 >+> p1

infixl 7 >+>
(>+>) :: Has Weave fs m
      => ( b' -> Woven fs a' a b' b m r)
      -> (_c' -> Woven fs b' b c' c m r)
      ->  _c' -> Woven fs a' a c' c m r
(fb' >+> fc') c' = fb' +>> fc' c'

infixl 6 <<+
(<<+) :: forall fs m a' a b' b c' c r. Has Weave fs m
      =>        Woven fs b' b c' c m r
      -> (b' -> Woven fs a' a b' b m r)
      ->        Woven fs a' a c' c m r
p <<+ fb = fb +>> p

infixr 6 +>>
(+>>) :: forall fs m a' a b' b c' c r. Has Weave fs m
      => (b' -> Woven fs a' a b' b m r)
      ->        Woven fs b' b c' c m r
      ->        Woven fs a' a c' c m r
fb' +>> p0 = \up dn -> transform (getScope up) up dn (p0 (unsafeCoerce up) dn)
  where
    transform scope up dn p1 = go p1
      where
        go = goRight (\b' -> fb' b' (unsafeCoerce up) (unsafeCoerce dn))
          where
            goRight :: (b' -> PlanT fs m r) -> PlanT fs m r -> PlanT fs m r
            goRight fb' = goRight'
              where
                goRight' p =
                  case p of
                    Step sym bp ->
                      case prj sym of
                        Just x ->
                          case x of
                            Request i b' _ ->
                              if i == scope
                              then goLeft (unsafeCoerce bp) (fb' (unsafeCoerce b'))
                              else Step sym (\b -> goRight' (bp b))
                            _ -> Step sym (\b -> goRight' (bp b))
                        Nothing -> Step sym (\b -> goRight' (bp b))
                    M m -> M (fmap goRight' m)
                    Pure r -> Pure r
            goLeft :: (b -> PlanT fs m r) -> PlanT fs m r -> PlanT fs m r
            goLeft bp = goLeft'
              where
                goLeft' p =
                  case p of
                    Step sym bp' ->
                      case prj sym of
                        Just x ->
                          case x of
                            Respond i b _ ->
                              if i == scope
                              then goRight (unsafeCoerce bp') (bp (unsafeCoerce b))
                              else Step sym (\b' -> goLeft' (bp' b'))
                            _ -> Step sym (\b' -> goLeft' (bp' b'))
                        Nothing -> Step sym (\b' -> goLeft' (bp' b'))
                    M m -> M (fmap goLeft' m)
                    Pure r -> Pure r

--------------------------------------------------------------------------------
-- Kleisli

infixl 1 >==
(>==) :: Has Weave fs m => Woven fs a' a b' b m r -> (r -> Woven fs a' a b' b m s) -> Woven fs a' a b' b m s
(>==) r rs = \up dn -> (r (unsafeCoerce up) (unsafeCoerce dn)) >>= \r -> (rs r up dn)

infixr 1 ==<
(==<) :: Has Weave fs m => (r -> Woven fs a' a b' b m s) -> Woven fs a' a b' b m r -> Woven fs a' a b' b m s
(==<) rs r = (>==) r rs

infixr 1 >==>
(>==>) :: Has Weave fs m => (t -> Woven fs a' a b' b m r) -> (r -> Woven fs a' a b' b m s) -> t -> Woven fs a' a b' b m s
(>==>) r rs x = r x >== rs

infixr 1 <==<
(<==<) :: Has Weave fs m => (r -> Woven fs a' a b' b m s) -> (t -> Woven fs a' a b' b m r) -> t -> Woven fs a' a b' b m s
(tr <==< rs) t = (rs >==> tr) t
