module Effect.Interleave where

import Mop.Core
import Data.Queue

import Unsafe.Coerce

-- this appears to work correctly but is in need of extensive testing

data Interleave k
  = forall fs m a. Fork Int (Plan fs m a)
  | forall fs m a. Atomically Int (Plan fs m a)
  | Stop Int
  | FreshScope (Int -> k)

data Interleaving k = Interleaving Int k
interleaves :: Uses Interleaving gs m => Attribute Interleaving gs m
interleaves = Interleaving 0 $ \fs ->
  let Interleaving i k = (fs&)
      i' = succ i
  in i' `seq` pure $ fs .= Interleaving i' k

instance Pair Interleaving Interleave where
  pair p (Interleaving i k) (FreshScope ik) = p k (ik i)

-- interleave $ \fork atomically ->
interleave :: forall fs m a. Has Interleave fs m
           => (    (forall b. Plan fs m b -> Plan fs m ())
                -> (forall b. Plan fs m b -> Plan fs m b)
                -> Plan fs m a
              ) -> Plan fs m a
interleave f = do
  scope <- self (FreshScope id)
  scoped scope $
      (f (\p -> self (Fork scope (p >> self (Stop scope))) >> Pure ())
         (\p -> self (Atomically scope p))
      )
  where
    scoped scope = rooted emptyQueue
      where
        rooted rest p =
          case p of
            Step sym bp ->
              case prj sym of
                Just x ->
                  case x of
                    -- fork is seen as a no-op to all scopes;
                    -- it doesn't take a slot out of any scope's time-slice
                    Fork i child ->
                      if i == scope
                      then rooted (enqueue (unsafeCoerce child) rest) (bp (unsafeCoerce ()))
                      else Step sym $ \b -> rooted rest (bp b)
                    Atomically i atom ->
                      if i == scope
                      then rooted emptyQueue (unsafeCoerce atom) >>= \b ->
                             case dequeue rest of
                               Nothing -> rooted rest (bp (unsafeCoerce b))
                               Just (rest',nxt) -> rooted (enqueue (bp (unsafeCoerce b)) rest') nxt
                      else Step sym $ \b -> rooted rest (bp b)
                    ~(Stop i) ->
                      if i == scope
                      then case dequeue rest of
                             Just (rest',nxt) -> rooted rest' nxt
                             Nothing -> Pure (unsafeCoerce ())
                      else Step sym $ \b -> rooted rest (bp b)
                Nothing -> Step sym $ \b ->
                  case dequeue rest of
                    Nothing -> rooted rest (bp b)
                    Just (rest',nxt) -> Step sym $ \b -> rooted (enqueue (bp b) rest') nxt
            M m -> M (m >>= \p' ->
                        case dequeue rest of
                          Nothing -> return $ rooted rest p'
                          Just (rest',nxt) -> return $ rooted (enqueue (unsafeCoerce p') rest') nxt
                     )
            Pure r ->
              case dequeue rest of
                Nothing -> Pure r
                Just (rest,nxt) -> rooted rest nxt >> Pure r
