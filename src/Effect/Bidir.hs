module Effect.Bidir where

import Mop

import Control.Monad
import Data.Function
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

data Switches k
  = FreshScope (Integer -> k)

  -- upstream interface
  | forall a'. Respond Integer a'
  | Await Integer k

  -- downstream interface
  | Request Integer k
  | forall b. Yield Integer b

data Switching k = Switching (IORef Integer) k

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
            Await i _ -> i
            Request i _ -> i
            Yield i _ -> i
            _ -> error "getScope got FreshScope"
        _ -> error "getScope got non-Switching"
    _ -> error "getScope got non-step"


{-# INLINE switching #-}
switching :: Uses Switching fs m => Instruction Switching fs m
switching = Switching (unsafePerformIO (newIORef 0)) $ \fs ->
  let Switching n k = view fs
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

-- respond await request yield
type Switcher fs a' a b' b m r
  =  (a' -> PlanT fs m ())
  -> PlanT fs m a
  -> PlanT fs m b'
  -> (b -> PlanT fs m ())
  -> PlanT fs m r
switcher :: Has Switches fs m
         => ((a' -> PlanT fs m ()) -> PlanT fs m a -> PlanT fs m b' -> (b -> PlanT fs m ()) -> PlanT fs m r) -> Switcher fs a' a b' b m r
switcher f = \rsp awt req yld ->
  let scp = getScope awt
  in f (\a' -> symbol (Respond scp a'))
       (symbol (Await scp undefined))
       (symbol (Request scp undefined))
       (\b -> symbol (Yield scp b))

type Composition fs m r = Switcher fs X X X X m r

conduct :: Has Switches fs m => Composition fs m r -> PlanT fs m r
conduct c = do
  scope <- freshScope
  c (\a' -> symbol (Respond scope a'))
    (symbol (Await scope undefined))
    (symbol (Request scope undefined))
    (\b -> symbol (Yield scope b))

(>-->) :: forall fs a' a b' b c' c m r. Has Switches fs m
       => Switcher fs a' a b' b m r
       -> Switcher fs b' b c' c m r
       -> Switcher fs a' a c' c m r
pl >--> pr = \rsp awt req yld -> transform (getScope awt)
                                   (pl rsp awt (unsafeCoerce req) (unsafeCoerce yld))
                                   (pr (unsafeCoerce rsp) (unsafeCoerce awt) req yld)
  where
    transform scope = start
      where
        start l r = go l
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i _ ->
                          if i == scope
                          then requesting (unsafeCoerce bp) r
                          else Step sym (\b -> go (bp b))
                        Yield i b ->
                          if i == scope
                          then yielding (bp (unsafeCoerce ())) (unsafeCoerce b) r
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        requesting a'l = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i a' ->
                          if i == scope
                          then start (a'l (unsafeCoerce a')) (bp (unsafeCoerce ()))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        yielding l b_ = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Await i _ ->
                          if i == scope
                          then start l (bp (unsafeCoerce b_))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

-- add Lazy/Strict as primitives; add them to yield/await/request/respond...
-- yield Lazy ..
-- yield Strict ..
-- request Lazy
-- request Strict
-- await Lazy
-- await Strict
-- respond Lazy ..
-- respond Strict ..

-- fully eager evaluation: evaluate the left side as far as we can while
-- maintaining the streaming properties and then switch sides and evaluate
-- as much of the right side as we can while maintaining the streaming
-- properties.
(>==>) :: forall fs a' a b' b c' c m r. Has Switches fs m
       => Switcher fs a' a b' b m r
       -> Switcher fs b' b c' c m r
       -> Switcher fs a' a c' c m r
pl >==> pr = \rsp awt req yld -> transform (getScope awt)
                                   (pl rsp awt (unsafeCoerce req) (unsafeCoerce yld))
                                   (pr (unsafeCoerce rsp) (unsafeCoerce awt) req yld)
  where
    transform scope = goLeft
      where
        goLeftWith b' bp = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        _ -> undefined

        goLeft l r = go l
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i _ ->
                          if i == scope
                          -- cannot continue without a value; have to evaluate right
                          then goRight bp r
                          else Step sym (\b -> go (bp b))
                        Yield i b ->
                          if i == scope
                          then continueLeft r b (bp (unsafeCoerce ()))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

        continueLeft r b = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i _ ->
                          if i == scope
                          -- cannot continue without a value; have to evaluate right
                          then requesting b bp r
                          else Step sym (\b -> go (bp b))
                        Yield i _ ->
                          if i == scope
                          -- second yield, we must switch threads to maintain streaming
                          then yielding b p r
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

        requesting b bpl = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        _ -> undefined

        yielding b pl = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        _ -> undefined

        goRightWith b bp = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Request i _ ->
                          if i == scope
                          then _
                          else Step sym (\b -> go (bp b))
                        Yield i _ ->
                          if i == scope
                          then _
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

        goRight bpl = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Respond i b' ->
                          if i == scope
                          then continueRight (bpl (unsafeCoerce b')) (bp (unsafeCoerce ()))
                          else Step sym (\b -> go (bp b))
                        Await i a ->
                          if i == scope
                          then error "Awaiting while upstream is requesting."
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
                        Respond i b' ->
                          if i == scope
                          then responding b' p l
                          else Step sym (\b -> go (bp b))
                        Await i a ->
                          if i == scope
                          then awaiting a bp l
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

        awaiting a bpr = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        _ -> undefined
                M m -> M (fmap go m)
                Pure r -> Pure r

        responding b' pr = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        _ -> undefined
                M m -> M (fmap go m)
                Pure r -> Pure r


(<--<) :: forall fs a' a b' b c' c m r. Has Switches fs m
       => Switcher fs a' a b' b m r
       -> Switcher fs b' b c' c m r
       -> Switcher fs a' a c' c m r
pl <--< pr = \rsp awt req yld -> transform (getScope awt)
                                   (pl rsp awt (unsafeCoerce req) (unsafeCoerce yld))
                                   (pr (unsafeCoerce rsp) (unsafeCoerce awt) req yld)
  where
    transform scope = start
      where
        start l r = go r
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Await i _ ->
                          if i == scope
                          then awaiting (unsafeCoerce bp) l
                          else Step sym (\b -> go (bp b))
                        Respond i b' ->
                          if i == scope
                          then responding (bp (unsafeCoerce ())) (unsafeCoerce b') l
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        awaiting bl = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Yield i b ->
                          if i == scope
                          then start (bl (unsafeCoerce b)) (bp (unsafeCoerce ()))
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r
        responding r b'_ = go
          where
            go p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Await i _ ->
                          if i == scope
                          then start (bp (unsafeCoerce b'_)) r
                          else Step sym (\b -> go (bp b))
                        _ -> Step sym (\b -> go (bp b))
                    Nothing -> Step sym (\b -> go (bp b))
                M m -> M (fmap go m)
                Pure r -> Pure r

instance Pair Switching Switches where
  pair p (Switching i k) (FreshScope ik) =
    let n = unsafePerformIO (readIORef i)
    in n `seq` p k (ik n)
  pair _ _ _ = error "Switching primitive escaped its scope:\n\
                     \\tAttempting to reuse control flow\
                     \ primitives outside of their scope\
                     \ is unsupported."

main = do
  delta (Instructions $ switching *:* Empty) (conduct $ stdinLn >--> takeWhile' (/= "quit") >--> stdoutLn)

stdinLn :: Has Switches fs IO => Switcher fs X X Bool String IO ()
stdinLn = switcher go
  where
    go _ _ req yld = go'
      where
        go' = do
          fromUp <- req
          case fromUp of
            False -> lift (putStrLn "Done in stdinLn")
            True -> do
              ln <- lift getLine
              yld ln
              go'

takeWhile' :: Has Switches fs m => (String -> Bool) -> Switcher fs Bool String X String m ()
takeWhile' pred = switcher go
  where
    go rsp awt _ yld = go'
      where
        go' = do
          rsp True
          str <- awt
          if pred str
          then do
            yld str
            go'
          else do
            yld str
            rsp False

stdoutLn :: Has Switches fs IO => Switcher fs X String X X IO r
stdoutLn = switcher go
  where
    go _ awt _ _ = go'
      where
        go' = do
          str <- awt
          lift (putStrLn str)
          go'
