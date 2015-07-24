{-# LANGUAGE TemplateHaskell #-}
module Example where

import Mop
import Generate

data OpenA k = OpenA k
data OpenB k = OpenB k
type OpenAB = OpenA :+: OpenB

-- make ''OpenA
deriving instance Functor OpenA
openA = liftF (inj (OpenA ()))
-- make ''OpenB
deriving instance Functor OpenB
openB = liftF (inj (OpenB ()))

-- makeCo ''OpenA
data CoOpenA k = CoOpenA k
deriving instance Functor CoOpenA
instance Pairing CoOpenA OpenA where
  pair p (CoOpenA k) (OpenA nxt) = p k nxt
coOpenA = CoOpenA

-- makeCo ''OpenB
data CoOpenB k = CoOpenB k
deriving instance Functor CoOpenB
instance Pairing CoOpenB OpenB where
  pair p (CoOpenB k) (OpenB nxt) = p k nxt
coOpenB = CoOpenB

-- open ''OpenAB
type CoOpenAB = CoOpenA :*: CoOpenB
coOpenAB = coOpenA *:* coOpenB

--------------------------------------------------------------------------------


data ClosedAB k
  = ClosedA k
  | ClosedB k


-- make ''ClosedAB
deriving instance Functor ClosedAB
closedA = liftF (inj (ClosedA ()))
closedB = liftF (inj (ClosedB ()))

-- makeCo ''ClosedAB
data CoClosedAB k = CoClosedAB
  { coClosedA :: k
  , coClosedB :: k
  } deriving (Functor)
instance Pairing CoClosedAB ClosedAB where
  pair p (CoClosedAB a _) (ClosedA k) = p a k
  pair p (CoClosedAB _ b) (ClosedB k) = p a k

-- closed ''ClosedAB
coClosedAB wa =
  let coClosedA = wa
      coClosedB = wa
  in CoClosedAB{..}
