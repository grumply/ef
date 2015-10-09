{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
module Effect.Pipes where

import Mop
import Unsafe.Coerce

import Control.Monad

import Prelude hiding (fail)

data Flow k
  = FreshScope (Integer -> k)
  | forall o. Yield Integer o
  | forall i. Await Integer (i -> k)
  | forall o. Respond Integer o
  | forall i. Request Integer (i -> k)

data Pipes k = Pipes Integer k
pipes :: Uses Pipes fs m => Instruction Pipes fs m
pipes = Pipes 0 $ \fs ->
  let Pipes n k = view fs
  in instruction (Pipes (succ n) k) fs

type Proxy fs a' a b' b m r
  =  (a' -> Plan fs m ()) -- respond
  -> Plan fs m a          -- await
  -> (b' -> Plan fs m ()) -- request
  -> Plan fs m b          -- yield
  -> Plan fs m r
{-
 Upstream | Downstream
     +---------+
     |         |
 a' <==       <== b'
     |    m    |
 a  ==>       ==> b
     |    |    |
     +----|----+
          v
          r
-}
newtype X = X X

closed :: X -> a
closed (X x) = closed x
{-# INLINABLE closed #-}

infixl 7 >->
(>->) :: Has Flow fs m
      => Proxy fs a' a () b m r
      -> Proxy fs () b c' c m r
      -> Proxy fs a' a c' c m r
(>->) = undefined

infixr 5 >~
(>~) :: Has Flow fs m
     => Proxy fs a' a y' y m b
     -> Proxy fs () b y' y m c
     -> Proxy fs a' a y' y m c
(>~) = undefined

infixr 4 ~>
(~>) :: Has Flow fs m
     => (a -> Proxy fs x' x b' b m a')
     -> (b -> Proxy fs x' x c' c m b')
     ->  a -> Proxy fs x' x c' c m a'
(~>) = undefined

infixr 7 <-<
(<-<) :: Has Flow fs m
      => Proxy fs () b c' c m r
      -> Proxy fs a' a () b m r
      -> Proxy fs a' a c' c m r
(<-<) = undefined

infixl 5 ~<
(~<) :: Has Flow fs m
     => Proxy fs () b y' y m c
     -> Proxy fs a' a y' y m b
     -> Proxy fs a' a y' y m c
(~<) = undefined

infixl 4 <~
(<~) :: Has Flow fs m
     => (b -> Proxy fs x' x c' c m b')
     -> (a -> Proxy fs x' x b' b m a')
     ->  a -> Proxy fs x' x c' c m a'
(<~) = undefined

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

infixr 4 />/
(/>/) :: Has Flow fs m
      => (a -> Proxy fs x' x b' b m a')
      -> (b -> Proxy fs x' x c' c m b')
      ->  a -> Proxy fs x' x c' c m a'
(/>/) = undefined

infixl 3 //>
(//>) :: Has Flow fs m
      =>       Proxy fs x' x b' b m a'
      -> (b -> Proxy fs x' x c' c m b')
      ->       Proxy fs x' x c' c m a'
(//>) = undefined

infixl 5 \>\
(\>\) :: Has Flow fs m
      => (b' -> Proxy fs a' a y' y m b)
      -> (c' -> Proxy fs b' b y' y m c)
      ->  c' -> Proxy fs a' a y' y m c
(\>\) = undefined

infixr 4 >\\
(>\\) :: Has Flow fs m
      => (b' -> Proxy fs a' a y' y m b)
      ->        Proxy fs b' b y' y m c
      ->        Proxy fs a' a y' y m c
(>\\) = undefined

infixr 8 >~>
(>~>) :: Has Flow fs m
      => (_a -> Proxy fs a' a b' b m r)
      -> ( b -> Proxy fs b' b c' c m r)
      ->  _a -> Proxy fs a' a c' c m r
(>~>) = undefined

infixl 7 >>~
(>>~) :: Has Flow fs m
      =>       Proxy fs a' a b' b m r
      -> (b -> Proxy fs b' b c' c m r)
      ->       Proxy fs a' a c' c m r
(>>~) = undefined

infixl 7 >+>
(>+>) :: Has Flow fs m
      => ( b' -> Proxy fs a' a b' b m r)
      -> (_c' -> Proxy fs b' b c' c m r)
      ->  _c' -> Proxy fs a' a c' c m r
(>+>) = undefined

infixr 6 +>>
(+>>) :: Has Flow fs m
      => (b' -> Proxy fs a' a b' b m r)
      ->        Proxy fs b' b c' c m r
      ->        Proxy fs a' a c' c m r
(+>>) = undefined

infixl 4 \<\
(\<\) :: Has Flow fs m
      => (b -> Proxy fs x' x c' c m b')
      -> (a -> Proxy fs x' x b' b m a')
      ->  a -> Proxy fs x' x c' c m a'
(\<\) = undefined

infixr 5 /</
(/</) :: Has Flow fs m
      => (c' -> Proxy fs b' b x' x m c)
      -> (b' -> Proxy fs a' a x' x m b)
      ->  c' -> Proxy fs a' a x' x m c
(/</)= undefined

infixl 8 <~<
(<~<) :: Has Flow fs m
      => (b -> Proxy fs b' b c' c m r)
      -> (a -> Proxy fs a' a b' b m r)
      ->  a -> Proxy fs a' a c' c m r
(<~<) = undefined

infixr 7 ~<<
(~<<) :: Has Flow fs m
      => (b -> Proxy fs b' b c' c m r)
      ->       Proxy fs a' a b' b m r
      ->       Proxy fs a' a c' c m r
(~<<) = undefined

infixr 7 <+<
(<+<) :: Has Flow fs m
      => (c' -> Proxy fs b' b c' c m r)
      -> (b' -> Proxy fs a' a b' b m r)
      ->  c' -> Proxy fs a' a c' c m r
(<+<) = undefined

infixr 3 <\\
(<\\) :: Has Flow fs m
      => (b -> Proxy fs x' x c' c m b')
      ->       Proxy fs x' x b' b m a'
      ->       Proxy fs x' x c' c m a'
(<\\) = undefined

reflect :: Has Flow fs m => Proxy fs a' a b' b m r -> Proxy fs b b' a a' m r
reflect = undefined

-- pipe :: forall fs a' a b' b m r. Has Pipe fs m
--      => (    (a -> Plan fs m ()) -- yield
--           -> Plan fs m b         -- request
--           -> (b -> Plan fs m ()) -- respond
--           -> Plan fs m a         -- await
--           -> Plan fs m r
--         )
--      -> (    (b -> Plan fs m ()) -- respond
--           -> Plan fs m a         -- await
--           -> Plan fs m r
--         )
--      -> Plan fs m r
-- pipe up down = do
--   scope <- symbol (FreshScope id)
--   link scope (up   (symbol . Yield   scope) (symbol (Request scope id)))
--              (down (symbol . Respond scope) (symbol (Await   scope id)))
--   where
--     link scope up0 down0 = go up0 down0
--       where
--         go _ _ = undefined

        -- go :: Plan fs m r -> Plan fs m r -> Plan fs m r
        -- go p c =
        --   case p of
        --     Step syms bp ->
        --       case prj syms of
        --         Just (Yield i a) ->
        --           if i == scope
        --           then inject (unsafeCoerce a) (bp (unsafeCoerce ())) c
        --           else Step syms (\b -> go (bp b) c)
        --         _ -> Step syms (\b -> go (bp b) c)
        --     M m -> M (fmap (flip go c) m)
        --     Pure r -> Pure r
        -- inject :: a -> Plan fs m r -> Plan fs m r -> Plan fs m r
        -- inject a p c =
        --   case c of
        --     Step syms bp ->
        --       case prj syms of
        --         Just (Await i ak) ->
        --           if i == scope
        --           then go p (bp (ak (unsafeCoerce a)))
        --           else Step syms (\b -> inject a p (bp b))
        --         _ -> Step syms (\b -> inject a p (bp b))
        --     M m -> M (fmap (inject a p) m)
        --     Pure r -> Pure r


instance Pair Pipes Flow where
  pair p (Pipes i k) (FreshScope ik) = p k (ik i)
  pair _ _ _ = error "Pipe primitive escaped its scope:\n\
                     \\t\tAttempting to reuse control flow\
                     \ primitives outside of their scope\
                     \ is unsupported."

comp :: Instructions '[Pipes] IO
comp = Instructions $ pipes *:* Empty

prod yield = do
  forM_ [(1 :: Int)..] $ \n -> do
    lift (putStrLn $ "Sending: " ++ show n)
    yield n
  return undefined


mid await yield = do
  n0 <- await
  lift (putStrLn $ "Got: " ++ show n0)
  n1 <- await
  lift (putStrLn $ "Got: " ++ show n1)
  yield (show [n0,n1])

cons await = do
  str <- await
  lift (putStrLn str)
  return "Done"

main :: IO ()
main = do
  -- (_,str) <- delta comp
  -- putStrLn str
  return ()

-- consumer is not productive enough because we immediately go back to the
-- producer after fulfilling an await. Need to walk to the next await and then
-- go back to the producer.
