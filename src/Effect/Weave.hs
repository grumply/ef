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
  , (<\\), (\<\), (~>),  (<~) , (/>/), (//>)
  , (\\<), (/</), (>~),  (~<) , (\>\), (>\\)
  ,        (<~<), (~<<), (>>~), (>~>)
  , (<<+), (<+<), (<-<), (>->), (>+>), (+>>)
  ,       (<==<), (==<), (>==), (>==>)
  ) where

import Mop hiding (push,pull)
import Data.Function
import Unsafe.Coerce

-- linearization of communicating threads

data Weave k
  = FreshScope (Integer -> k)
  | forall fs a' a m r. Request Integer a' (a  -> Plan fs m r)
  | forall fs b' b m r. Respond Integer b  (b' -> Plan fs m r)

data Weaving k = Weaving Integer k

freshScope :: Has Weave fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

weaving :: Uses Weaving fs m => Instruction Weaving fs m
weaving = Weaving 0 $ \fs ->
  let Weaving n k = view fs
  in instruction (Weaving (succ n) k) fs

getScope p =
  case p undefined undefined of
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
  pair p (Weaving i k) (FreshScope ik) = p k (ik i)
  pair _ _ _ = error "Pipe primitive escaped its scope:\n\
                     \\tAttempting to reuse control flow\
                     \ primitives outside of their scope\
                     \ is unsupported."

linearize :: Has Weave fs m => Effect fs m r -> Plan fs m r
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
{-# INLINABLE linearize #-}

newtype X = X X

closed :: X -> a
closed (X x) = closed x
{-# INLINABLE closed #-}

type Effect fs m r = Woven fs X () () X m r

type Producer fs b m r = Woven fs X () () b m r
-- producer $ \yield -> do { .. ; }
producer :: Has Weave fs m => ((b -> Plan fs m ()) -> Plan fs m r) -> Producer' fs b m r
producer f = \_ dn -> f (\b -> symbol (Respond (getScope dn) b Pure))

type Consumer fs a m r = Woven fs () a () X m r
-- consumer $ \await -> do { .. ; }
consumer :: Has Weave fs m => (Plan fs m a -> Plan fs m r) -> Consumer' fs a m r
consumer f = \up _ -> f (symbol (Request (getScope up) () Pure))

type Pipe fs a b m r = Woven fs () a () b m r
-- pipe $ \await yield -> do { .. ; }
pipe :: Has Weave fs m => (Plan fs m a -> (b -> Plan fs m x) -> Plan fs m r) -> Pipe fs a b m r
pipe f = \up dn -> f (symbol (Request (getScope up) () Pure))
                     (\b -> symbol (Respond (getScope dn) b Pure))

type Client fs a' a m r = Woven fs a' a () X m r
-- client $ \request -> do { .. ; }
client :: Has Weave fs m => ((a -> Plan fs m a') -> Plan fs m r) -> Client' fs a' a m r
client f = \up _ -> f (\a -> symbol (Request (getScope up) a Pure))

type Server fs b' b m r = Woven fs X () b' b m r
-- server $ \respond -> do { .. ; }
server :: Has Weave fs m => ((b' -> Plan fs m b) -> Plan fs m r) -> Server' fs b' b m r
server f = \_ dn -> f (\b' -> symbol (Respond (getScope dn) b' Pure))

type Woven fs a' a b' b m r
  =  (forall x. a' -> (a -> Plan fs m x) -> Plan fs m x)
  -> (forall x. b -> (b' -> Plan fs m x) -> Plan fs m x)
  -> Plan fs m r
-- weave $ \request respond -> do { .. ; }
weave :: Has Weave fs m => ((a -> Plan fs m a') -> (b' -> Plan fs m b) -> Plan fs m r) -> Woven fs a' a b' b m r
weave f = \up dn -> f (\a -> symbol (Request (getScope up) a Pure))
                      (\b' -> symbol (Respond (getScope dn) b' Pure))

type Effect' fs m r = forall x' x y' y . Woven fs x' x y' y m r

type Producer' fs b m r = forall x' x . Woven fs x' x () b m r

type Consumer' fs a m r = forall y' y . Woven fs () a y' y m r

type Server' fs b' b m r = forall x' x . Woven fs x' x b' b m r

type Client' fs a' a m r = forall y' y . Woven fs a' a y' y m r


--------------------------------------------------------------------------------
-- Respond

-- convenient, but I'd rather use the name for generators
-- for :: Has Weave fs m
--     =>       Woven fs x' x b' b m a'
--     -> (b -> Woven fs x' x c' c m b')
--     ->       Woven fs x' x c' c m a'
-- for = (//>)
-- {-# INLINABLE for #-}

infixr 3 <\\
(<\\) :: Has Weave fs m
      => (b -> Woven fs x' x c' c m b')
      ->       Woven fs x' x b' b m a'
      ->       Woven fs x' x c' c m a'
f <\\ p = p //> f
{-# INLINABLE (<\\) #-}

infixl 4 \<\ --
(\<\) :: Has Weave fs m
      => (b -> Woven fs x' x c' c m b')
      -> (a -> Woven fs x' x b' b m a')
      ->  a -> Woven fs x' x c' c m a'
p1 \<\ p2 = p2 />/ p1
{-# INLINABLE (\<\) #-}

infixr 4 ~>
(~>) :: Has Weave fs m
     => (a -> Woven fs x' x b' b m a')
     -> (b -> Woven fs x' x c' c m b')
     ->  a -> Woven fs x' x c' c m a'
(~>) = (/>/)
{-# INLINABLE (~>) #-}

infixl 4 <~
(<~) :: Has Weave fs m
     => (b -> Woven fs x' x c' c m b')
     -> (a -> Woven fs x' x b' b m a')
     ->  a -> Woven fs x' x c' c m a'
g <~ f = f ~> g
{-# INLINABLE (<~) #-}

infixr 4 />/
(/>/) :: Has Weave fs m
      => (a -> Woven fs x' x b' b m a')
      -> (b -> Woven fs x' x c' c m b')
      ->  a -> Woven fs x' x c' c m a'
(fa />/ fb) a = fa a //> fb
{-# INLINABLE (/>/) #-}

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
                      then go (bp $ unsafeCoerce $ fb (unsafeCoerce b) (unsafeCoerce up) (unsafeCoerce dn))
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r
{-# INLINABLE (//>) #-}

unfold :: ((b -> Plan fs m b') -> Plan fs m r) -> Woven fs a' a b' b m r
unfold u = \_ respond -> u (\x -> respond x Pure)
{-# INLINABLE unfold #-}

--------------------------------------------------------------------------------
-- Request


infixr 5 /</
(/</) :: Has Weave fs m
      => (c' -> Woven fs b' b x' x m c)
      -> (b' -> Woven fs a' a x' x m b)
      ->  c' -> Woven fs a' a x' x m c
p1 /</ p2 = p2 \>\ p1
{-# INLINABLE (/</) #-}

infixr 5 >~
(>~) :: Has Weave fs m
     => Woven fs a' a y' y m b
     -> Woven fs () b y' y m c
     -> Woven fs a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2
{-# INLINABLE (>~) #-}

infixl 5 ~<
(~<) :: Has Weave fs m
     => Woven fs () b y' y m c
     -> Woven fs a' a y' y m b
     -> Woven fs a' a y' y m c
p2 ~< p1 = p1 >~ p2
{-# INLINABLE (~<) #-}

infixl 5 \>\ --
(\>\) :: Has Weave fs m
      => (b' -> Woven fs a' a y' y m b)
      -> (c' -> Woven fs b' b y' y m c)
      ->  c' -> Woven fs a' a y' y m c
(fb' \>\ fc') c' = fb' >\\ fc' c'
{-# INLINABLE (\>\) #-}

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
                      then go (bp (unsafeCoerce (fb' (unsafeCoerce b') (unsafeCoerce up) (unsafeCoerce dn))))
                      else Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                Nothing -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r
{-# INLINABLE (>\\) #-}

fold :: ((a' -> Plan fs m a) -> Plan fs m r) -> Woven fs a' a b' b m r
fold f = \request _ -> f (\x -> request x Pure)
{-# INLINABLE fold #-}

--------------------------------------------------------------------------------
-- Push

infixl 8 <~<
(<~<) :: Has Weave fs m
      => (b -> Woven fs b' b c' c m r)
      -> (a -> Woven fs a' a b' b m r)
      ->  a -> Woven fs a' a c' c m r
p1 <~< p2 = p2 >~> p1
{-# INLINABLE (<~<) #-}

infixr 8 >~>
(>~>) :: Has Weave fs m
      => (_a -> Woven fs a' a b' b m r)
      -> ( b -> Woven fs b' b c' c m r)
      ->  _a -> Woven fs a' a c' c m r
(fa >~> fb) a = fa a >>~ fb
{-# INLINABLE (>~>) #-}

infixr 7 ~<<
(~<<) :: Has Weave fs m
      => (b -> Woven fs b' b c' c m r)
      ->       Woven fs a' a b' b m r
      ->       Woven fs a' a c' c m r
k ~<< p = p >>~ k
{-# INLINABLE (~<<) #-}

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
            goLeft :: (b -> Plan fs m r) -> Plan fs m r -> Plan fs m r
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
            goRight :: (b' -> Plan fs m r) -> Plan fs m r -> Plan fs m r
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
{-# INLINABLE (>>~) #-}

--------------------------------------------------------------------------------
-- Pull

infixl 7 >->
(>->) :: Has Weave fs m
      => Woven fs a' a () b m r
      -> Woven fs () b c' c m r
      -> Woven fs a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2
{-# INLINABLE (>->) #-}

infixr 7 <-<
(<-<) :: Has Weave fs m
      => Woven fs () b c' c m r
      -> Woven fs a' a () b m r
      -> Woven fs a' a c' c m r
p2 <-< p1 = p1 >-> p2
{-# INLINABLE (<-<) #-}

infixr 7 <+<
(<+<) :: Has Weave fs m
      => (c' -> Woven fs b' b c' c m r)
      -> (b' -> Woven fs a' a b' b m r)
      ->  c' -> Woven fs a' a c' c m r
p1 <+< p2 = p2 >+> p1
{-# INLINABLE (<+<) #-}

infixl 7 >+>
(>+>) :: Has Weave fs m
      => ( b' -> Woven fs a' a b' b m r)
      -> (_c' -> Woven fs b' b c' c m r)
      ->  _c' -> Woven fs a' a c' c m r
(fb' >+> fc') c' = fb' +>> fc' c'
{-# INLINABLE (>+>) #-}

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
            goRight :: (b' -> Plan fs m r) -> Plan fs m r -> Plan fs m r
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
            goLeft :: (b -> Plan fs m r) -> Plan fs m r -> Plan fs m r
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
{-# INLINABLE (+>>) #-}

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
