{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
module Effect.Pipes where

import Mop

import Data.Function
import Control.Monad
import Unsafe.Coerce

data Flow k
  = FreshScope (Integer -> k)
  | forall fs a' a m r. Request Integer a' (a  -> Plan fs m r)
  | forall fs b' b m r. Respond Integer b  (b' -> Plan fs m r)

getScope :: forall fs m r. Has Flow fs m => Plan fs m r -> Integer
getScope f =
  let err = error "getScope: Shouldn't happen. Expecting a properly \
                  \scoped Flow communication primitive."
  in case f of
       Step sym _ ->
         case prj ((unsafeCoerce sym) :: Symbol fs z) of
           Just x ->
             case x of
               Request i _ _ -> i
               Respond i _ _ -> i
               _ -> err
           _ -> err

freshScope :: Has Flow fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

data Pipes k = Pipes Integer k

pipes :: Uses Pipes fs m => Instruction Pipes fs m
pipes = Pipes 0 $ \fs ->
  let Pipes n k = view fs
  in instruction (Pipes (succ n) k) fs

type Proxy fs a' a b' b m r
  =  (forall x. a' -> (a -> Plan fs m x) -> Plan fs m x)
  -> (forall x. b -> (b' -> Plan fs m x) -> Plan fs m x)
  -> Plan fs m r

flow :: Has Flow fs m => Effect fs m r -> Plan fs m r
flow e = do
    scope <- freshScope
    transform scope $ e (\_ _ -> symbol (Request scope undefined undefined))
                        (\_ _ -> symbol (Respond scope undefined undefined))
  where
    transform scope p0 = go p0
      where
        go p =
          case p of
            Step sym bp ->
              case prj sym of
                Just (Request i a' _) ->
                  if i == scope
                  then closed (unsafeCoerce a')
                  else Step sym (\b -> go (bp b))
                Just (Respond i b _) ->
                  if i == scope
                  then closed (unsafeCoerce b)
                  else Step sym (\b -> go (bp b))
                _ -> Step sym (\b -> go (bp b))
            M m -> M (fmap go m)
            Pure r -> Pure r
{-# INLINABLE flow #-}

newtype X = X X

closed :: X -> a
closed (X x) = closed x
{-# INLINABLE closed #-}

type Effect fs m r = Proxy fs X () () X m r

type Producer fs b m r = Proxy fs X () () b m r

type Pipe fs a b m r = Proxy fs () a () b m r

type Consumer fs a m r = Proxy fs () a () X m r

type Client fs a' a m r = Proxy fs a' a () X m r

type Server fs b' b m r = Proxy fs X () b' b m r

type Effect' fs m r = forall x' x y' y . Proxy fs x' x y' y m r

type Producer' fs b m r = forall x' x . Proxy fs x' x () b m r

type Consumer' fs a m r = forall y' y . Proxy fs () a y' y m r

type Server' fs b' b m r = forall x' x . Proxy fs x' x b' b m r

type Client' fs a' a m r = forall y' y . Proxy fs a' a y' y m r


--------------------------------------------------------------------------------
-- Respond

for :: Has Flow fs m
    =>       Proxy fs x' x b' b m a'
    -> (b -> Proxy fs x' x c' c m b')
    ->       Proxy fs x' x c' c m a'
for = (//>)

infixr 3 <\\
(<\\) :: Has Flow fs m
      => (b -> Proxy fs x' x c' c m b')
      ->       Proxy fs x' x b' b m a'
      ->       Proxy fs x' x c' c m a'
f <\\ p = p //> f

infixl 4 \<\ --
(\<\) :: Has Flow fs m
      => (b -> Proxy fs x' x c' c m b')
      -> (a -> Proxy fs x' x b' b m a')
      ->  a -> Proxy fs x' x c' c m a'
p1 \<\ p2 = p2 />/ p1

infixr 4 ~>
(~>) :: Has Flow fs m
     => (a -> Proxy fs x' x b' b m a')
     -> (b -> Proxy fs x' x c' c m b')
     ->  a -> Proxy fs x' x c' c m a'
(~>) = (/>/)

infixl 4 <~
(<~) :: Has Flow fs m
     => (b -> Proxy fs x' x c' c m b')
     -> (a -> Proxy fs x' x b' b m a')
     ->  a -> Proxy fs x' x c' c m a'
g <~ f = f ~> g

infixr 4 />/
(/>/) :: Has Flow fs m
      => (a -> Proxy fs x' x b' b m a')
      -> (b -> Proxy fs x' x c' c m b')
      ->  a -> Proxy fs x' x c' c m a'
(fa />/ fb) a = fa a //> fb

infixl 3 //>
(//>) :: forall fs x' x b' b c' c m a'. Has Flow fs m
      =>       Proxy fs x' x b' b m a'
      -> (b -> Proxy fs x' x c' c m b')
      ->       Proxy fs x' x c' c m a'
p0 //> fb = \_ _ -> do scope <- freshScope
                       transform scope p0
  where
    transform :: Integer -> Proxy fs x' x b' b m a' -> Plan fs m a'
    transform scope p0 =
        let request a' ap = symbol (Request scope a' ap)
            respond b b'p = symbol (Respond scope b b'p)
        in go request respond (p0 request respond)
      where
        go :: (a' -> (a -> Plan fs m r) -> Plan fs m r)
           -> (b -> (b' -> Plan fs m r) -> Plan fs m r)
           -> Plan fs m r
           -> Plan fs m r
        go req rsp = go'
          where
            go' :: Plan fs m r -> Plan fs m r
            go' p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i b b'p -> do
                          if i == scope
                          then do
                            b' <- fb (unsafeCoerce b)
                                     (unsafeCoerce req)
                                     (unsafeCoerce rsp)
                            go' (bp $ unsafeCoerce $ b'p (unsafeCoerce b'))
                          else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> Pure r

unfold :: Has Flow fs m => ((b -> Plan fs m b') -> Plan fs m r) -> Proxy fs a' a b' b m r
unfold u = \_ respond -> u (\x -> respond x Pure)

--------------------------------------------------------------------------------
-- Request


infixr 5 /</
(/</) :: Has Flow fs m
      => (c' -> Proxy fs b' b x' x m c)
      -> (b' -> Proxy fs a' a x' x m b)
      ->  c' -> Proxy fs a' a x' x m c
p1 /</ p2 = p2 \>\ p1

infixr 5 >~
(>~) :: Has Flow fs m
     => Proxy fs a' a y' y m b
     -> Proxy fs () b y' y m c
     -> Proxy fs a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2

infixl 5 ~<
(~<) :: Has Flow fs m
     => Proxy fs () b y' y m c
     -> Proxy fs a' a y' y m b
     -> Proxy fs a' a y' y m c
p2 ~< p1 = p1 >~ p2

infixl 5 \>\ --
(\>\) :: Has Flow fs m
      => (b' -> Proxy fs a' a y' y m b)
      -> (c' -> Proxy fs b' b y' y m c)
      ->  c' -> Proxy fs a' a y' y m c
(fb' \>\ fc') c' = fb' >\\ fc' c'

infixr 4 >\\ --
(>\\) :: forall fs y' y a' a b' b m c. Has Flow fs m
      => (b' -> Proxy fs a' a y' y m b)
      ->        Proxy fs b' b y' y m c
      ->        Proxy fs a' a y' y m c
fb' >\\ p0 = \_ _ -> do scope <- freshScope
                        transform scope p0
  where
    transform :: Integer -> Proxy fs b' b y' y m c -> Plan fs m c
    transform scope p1 =
        let request a' ap = symbol (Request scope a' ap)
            respond b b'p = symbol (Respond scope b b'p)
        in go request respond (p1 request respond)
      where
        go :: (a' -> (a -> Plan fs m r) -> Plan fs m r)
           -> (b -> (b' -> Plan fs m r) -> Plan fs m r)
           -> Plan fs m r
           -> Plan fs m r
        go req rsp = go'
          where
            go' :: Plan fs m r -> Plan fs m r
            go' p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i b' fb -> do
                          if i == scope
                          then do
                            b <- fb' (unsafeCoerce b')
                                     (unsafeCoerce req)
                                     (unsafeCoerce rsp)
                            go' (bp (unsafeCoerce (fb (unsafeCoerce b))))
                          else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> Pure r

fold :: Has Flow fs m => ((a' -> Plan fs m a) -> Plan fs m r) -> Proxy fs a' a b' b m r
fold f = \request _ -> f (\x -> request x Pure)

--------------------------------------------------------------------------------
-- Push

infixl 8 <~<
(<~<) :: Has Flow fs m
      => (b -> Proxy fs b' b c' c m r)
      -> (a -> Proxy fs a' a b' b m r)
      ->  a -> Proxy fs a' a c' c m r
p1 <~< p2 = p2 >~> p1

push :: Has Flow fs m => ((a -> Plan fs m r) -> Plan fs m r) -> Proxy fs a' a a' a m r
push f = \request respond -> f (fix $ \go -> \a -> respond a (\a' -> (request a' go)))

infixr 8 >~>
(>~>) :: Has Flow fs m
      => (_a -> Proxy fs a' a b' b m r)
      -> ( b -> Proxy fs b' b c' c m r)
      ->  _a -> Proxy fs a' a c' c m r
(fa >~> fb) a = fa a >>~ fb


infixr 7 ~<<
(~<<) :: Has Flow fs m
      => (b -> Proxy fs b' b c' c m r)
      ->       Proxy fs a' a b' b m r
      ->       Proxy fs a' a c' c m r
k ~<< p = p >>~ k

infixl 7 >>~
(>>~) :: forall fs a' a b' b c' c m r. Has Flow fs m
      =>       Proxy fs a' a b' b m r
      -> (b -> Proxy fs b' b c' c m r)
      ->       Proxy fs a' a c' c m r
p0 >>~ fb0 = \_ _ -> do scope <- freshScope
                        transform scope p0
  where
    transform :: Integer -> Proxy fs a' a b' b m r -> Plan fs m r
    transform scope p1 =
        let request a' ap = symbol (Request scope a' ap)
            respond b b'p = symbol (Respond scope b b'p)
        in go request respond (p1 request respond)
      where
        go :: (a' -> (a -> Plan fs m r) -> Plan fs m r)
           -> (b -> (b' -> Plan fs m r) -> Plan fs m r)
           -> Plan fs m r
           -> Plan fs m r
        go req rsp = goLeft (\b -> fb0 b (unsafeCoerce req) (unsafeCoerce rsp))
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
                              -- need to use bp' bp' b' bp''
                              then goLeft (unsafeCoerce bp) (b'p (unsafeCoerce b'))
                              else Step sym (\b -> goRight' (bp b))
                            _ -> Step sym (\b -> goRight' (bp b))
                        Nothing -> Step sym (\b -> goRight' (bp b))
                    M m -> M (fmap goRight' m)
                    Pure r -> Pure r

--------------------------------------------------------------------------------
-- Pull

infixl 7 >->
(>->) :: Has Flow fs m
      => Proxy fs a' a () b m r
      -> Proxy fs () b c' c m r
      -> Proxy fs a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2

infixr 7 <-<
(<-<) :: Has Flow fs m
      => Proxy fs () b c' c m r
      -> Proxy fs a' a () b m r
      -> Proxy fs a' a c' c m r
p2 <-< p1 = p1 >-> p2

infixr 7 <+<
(<+<) :: Has Flow fs m
      => (c' -> Proxy fs b' b c' c m r)
      -> (b' -> Proxy fs a' a b' b m r)
      ->  c' -> Proxy fs a' a c' c m r
p1 <+< p2 = p2 >+> p1

pull :: Has Flow fs m => ((a' -> Plan fs m r) -> Plan fs m r) -> Proxy fs a' a a' a m r
pull f = \request respond -> f (fix $ \go -> \a' -> request a' (\a -> (respond a go)))

infixl 7 >+>
(>+>) :: Has Flow fs m
      => ( b' -> Proxy fs a' a b' b m r)
      -> (_c' -> Proxy fs b' b c' c m r)
      ->  _c' -> Proxy fs a' a c' c m r
(fb' >+> fc') c' = fb' +>> fc' c'

infixr 6 +>>
(+>>) :: forall fs m a' a b' b c' c r. Has Flow fs m
      => (b' -> Proxy fs a' a b' b m r)
      ->        Proxy fs b' b c' c m r
      ->        Proxy fs a' a c' c m r
fb' +>> p0 = \_ _ -> do scope <- freshScope
                        transform scope p0
  where
    transform :: Integer -> Proxy fs b' b c' c m r -> Plan fs m r
    transform scope p1 =
        let request b' bp = symbol (Request scope b' bp)
            respond c c'p = symbol (Respond scope c c'p)
        in go request respond (p1 request respond)
      where
        go :: (b' -> (b -> Plan fs m r) -> Plan fs m r)
           -> (a -> (a' -> Plan fs m r) -> Plan fs m r)
           -> Plan fs m r
           -> Plan fs m r
        go req rsp = goRight (\b' -> fb' b' (unsafeCoerce req) (unsafeCoerce rsp))
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



--------------------------------------------------------------------------------

-- reflect :: Has Flow fs m => Proxy fs a' a b' b m r -> Proxy fs b b' a a' m r
-- reflect p0 = \rsp awt req yld -> go (p0 (unsafeCoerce rsp) (unsafeCoerce awt) (unsafeCoerce req) (unsafeCoerce yld))
--   where
--     go p =
--       case p of
--         Step sym bp ->
--           case prj sym of
--             Just x ->
--               case x of
--                 Yield o   -> Step (inj (Respond o)) (\b -> go (bp (unsafeCoerce b)))
--                 Respond o -> Step (inj (Yield   o)) (\b -> go (bp (unsafeCoerce b)))
--                 Request o -> Step (inj (Await   o)) (\b -> go (bp (unsafeCoerce b)))
--                 Await o   -> Step (inj (Request o)) (\b -> go (bp (unsafeCoerce b)))
--             Nothing -> Step sym (\b -> go (bp b))
--         M m -> M (fmap go m)
--         Pure r -> Pure r

instance Pair Pipes Flow where
  pair p (Pipes i k) (FreshScope ik) = p k (ik i)
  pair _ _ _ = error "Pipe primitive escaped its scope:\n\
                     \\t\tAttempting to reuse control flow\
                     \ primitives outside of their scope\
                     \ is unsupported."

comp :: Instructions '[Pipes] IO
comp = Instructions $ pipes *:* Empty

-- handler :: Has Flow fs IO => Int -> Proxy fs () b' b IO String
handler b = \_ _ -> do
  lift $ do
    putStrLn "Receiving in handler."
    putStrLn $ "Received: " ++ show b

-- prod :: Has Flow fs IO => Proxy fs a' a b' Int IO String
prod = unfold $ \respond -> do
  forM_ [(1 :: Int)..3] $ \n -> do
    lift (putStrLn $ "Sending in prod.")
    _ <- respond n
    lift $ do
      putStrLn $ "Sent "  ++ show n ++ " in prod."
      putStrLn ""

    return ()
  return "DoneProd"


main :: IO ()
main = do
  (_,str) <- delta comp $ flow (prod //> handler )
  putStrLn str
  return ()

-- consumer is not productive enough because we immediately go back to the
-- producer after fulfilling an await. Need to walk to the next await and then
-- go back to the producer.
