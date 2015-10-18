module Effect.Bidir where

import Mop

import Control.Monad
import Data.Function
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Maybe

data Switches k
  = FreshScope (Integer -> k)

  -- upstream interface
  | forall a'. Respond Integer a'
  | Await Integer

  -- downstream interface
  | Request Integer
  | forall b. Yield Integer b

data Switching k = Switching k (IORef Integer) k

newtype X = X X

closed :: X -> a
closed (X x) = closed x

{-# INLINABLE freshScope #-}
freshScope :: Has Switches fs m => PlanT fs m Integer
freshScope = symbol (FreshScope id)

getScope p =
  case p of
    Step sym bp ->
      case prj sym of
        Just x ->
          case x of
            Respond i _ -> i
            Await i -> i
            Request i -> i
            Yield i _ -> i
            _ -> error "getScope got FreshScope"
        _ -> error "getScope got non-Switching"
    _ -> error "getScope got non-step"


{-# INLINE switching #-}
switching :: Uses Switching fs m => Instruction Switching fs m
switching = Switching return (unsafePerformIO (newIORef 0)) $ \fs ->
  let Switching dn n k = view fs
      n' = unsafePerformIO (modifyIORef n succ)
  in n' `seq` return fs

{-
Upstream | Downstream
     +---------+
     |         |
 a' <==       <== b'
     |         |
 a  ==>       ==> b
     |    |    |
     +----|----+
          v
          r
-}


-- respond await request yield done
type Switcher fs a' a b' b m r
  =  (a' -> PlanT fs m ())   -- respond
  -> PlanT fs m a            -- await
  -> PlanT fs m b'           -- request
  -> (b -> PlanT fs m ())    -- yield
  -> PlanT fs m r

switcher :: Has Switches fs m
         => (    (a' -> PlanT fs m ())    -- respond
              -> PlanT fs m a             -- await
              -> PlanT fs m b'            -- request
              -> (b -> PlanT fs m ())     -- yield
              -> PlanT fs m r
            )
         -> Switcher fs a' a b' b m r
switcher f = \_ awt _ _ ->
  let scp = getScope awt
  in f (\a' -> symbol (Respond scp a'))
       (symbol (Await scp))
       (symbol (Request scp))
       (\b -> symbol (Yield scp b))

type Switched fs m r = Switcher fs X X X X m r

infixl 5 >->
-- yield-fast
(>->) :: Has Switches fs m
      => Switcher fs a' a b' b m r
      -> Switcher fs b' b c' c m r
      -> Switcher fs a' a c' c m r
l >-> r = \rsp awt req yld ->
  transform (getScope awt)
    (l rsp awt (unsafeCoerce req) (unsafeCoerce yld))
    (r (unsafeCoerce rsp) (unsafeCoerce awt) req yld)
  where
    transform scope l r = start l r
      where
        start l r = go l
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i ->
                          if i == scope
                          then requesting (unsafeCoerce bp) r
                          else Step sym (\b -> go (bp b))
                        Yield i _b ->
                          if i == scope
                          then yielding (unsafeCoerce _b) (bp (unsafeCoerce ())) r
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure res -> Pure res
        requesting bpl = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i a' ->
                          if i == scope
                          then start (bpl (unsafeCoerce a')) (bp (unsafeCoerce ()))
                          else Step sym (\b -> go (bp b))
                        Await i ->
                          if i == scope
                          then error "Request/Await deadlock"
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        yielding b l = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i a' ->
                          if i == scope
                          then error "Yield/Respond deadlock"
                          else Step sym (\b -> go (bp b))
                        Await i ->
                          if i == scope
                          then continueRight l (bp (unsafeCoerce b))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    _ -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure res -> Pure res
        continueRight l = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i a' ->
                          if i == scope
                          then _
                          else
                        Await i ->
                          if i == scope
                          then _
                          else Step sym (\b -> go (bp b))


conduct :: Has Switches fs m => Switched fs m r -> PlanT fs m r
conduct c = do
  scope <- freshScope
  c (\a' -> symbol (Respond scope a'))
    (symbol (Await scope))
    (symbol (Request scope))
    (\b -> symbol (Yield scope b))

instance Pair Switching Switches where
  pair p (Switching _ i k) (FreshScope ik) =
    let n = unsafePerformIO (readIORef i)
    in n `seq` p k (ik n)
  pair _ _ _ = error "Switching primitive escaped its scope:\n\
                     \\tAttempting to reuse control flow\
                     \ primitives outside of their scope\
                     \ is unsupported."

stdinLn = switcher go
  where
    go _ _ _ yld = go'
      where
        go' = do
          ln <- lift getLine
          lift (putStrLn "Yielding from stdinLn")
          yld ln
          go'

takeWhile' pred = switcher go
  where
    go _ awt _ yld = go'
      where
        go' = do
          lift (putStrLn "Awaiting in takeWhile'")
          ln <- awt
          lift (putStrLn "Yielding in takeWhile'")
          if pred ln
          then do
            yld ln
            return "takeWhile'"
          else do
            yld ln
            go'

stdoutLn = switcher go
  where
    go _ awt _ _ = go'
      where
        go' = do
          lift (putStrLn "Awaiting in stdoutLn")
          ln <- awt
          lift (putStrLn ln)
          go'

main = do
  (_,str) <- delta (Instructions $ switching *:* Empty) (conduct $ stdinLn >-> takeWhile' (/= "quit") >-> stdoutLn)
  putStrLn str
