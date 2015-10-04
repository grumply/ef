{-# LANGUAGE ExistentialQuantification #-}
{-
data Choose v = forall a. Choose [a] (a -> v)

choose :: Member Choose r => [a] -> Eff r a
choose lst = send . inj $ Choose lst id

mzero' :: Member Choose r => Eff r a
mzero' = choose []

mplus' :: Member Choose r => Eff r a -> Eff r a -> Eff r a
mplus' m1 m2 = join $ choose [m1,m2]

runChoice :: forall a r. Eff (Choose :> r) a -> Eff r [a]
runChoice = loop
 where
  loop = freeMap
         (\x -> return [x])
         (\u -> handleRelay u loop (\(Choose lst k) -> handle lst k))

  handle :: [t] -> (t -> Eff (Choose :> r) a) -> Eff r [a]
  handle [] _  = return []
  handle [x] k = loop (k x)
  handle lst k = concat `fmap` mapM (loop . k) lst
-}
module Effect.Choose where

import Mop

data Choose k = forall a. Choose [a] (a -> k)

choose :: Has Choose fs m => [a] -> Plan fs m a
choose lst = symbol (Choose lst id)

data Chooser k = forall a. Chooser [a] (a,k)

instance Pair Chooser Choose where
  pair p (Chooser as ak) (Choose as ak) =
