{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Continuation
  (callCC
  ,continuations,ContHandler(..)
  ) where

import Mop
import Unsafe.Coerce

data Cont k
  = FreshScope (Integer -> k)
  | forall a. Cont Integer a
data ContHandler k = ContHandler Integer k

callCC :: Has Cont fs m => ((forall b. a -> Plan fs m b) -> Plan fs m a) -> Plan fs m a
callCC x = do
    f <- freshScope
    transform f $ x (\a -> symbol (Cont f a))
  where
    transform f =
      mapStep $ \go (Step syms bp) ->
        case prj syms of
          Just (Cont i a) ->
            if i == f
            then Pure (unsafeCoerce a)
            else Step syms (\b -> go (bp b))
          _ -> Step syms (\b -> go (bp b))

freshScope :: Has Cont fs m => Plan fs m Integer
freshScope = symbol (FreshScope id)

continuations :: Uses ContHandler gs m => Instruction ContHandler gs m
continuations = ContHandler 0 $ \fs ->
  let ContHandler i k = view fs
  in instruction (ContHandler (succ i) k) fs

instance Pair ContHandler Cont where
  pair p (ContHandler i k) (FreshScope ik) = p k (ik i)
  pair p _ (Cont _ _) = error "Unscoped continuation."
