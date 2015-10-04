{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Effect.Capture where

import Mop
import Data.Function
import Control.Monad
import Unsafe.Coerce

data Capture k' k = GetCC (k' -> k) | SetCC k' k
data Reify k' k = Reify (k',k) (k' -> k)

recall :: forall fs m a.
         (Has (Capture (Cont m a)) (App m a) m)
      => Plan (App m a) m a
recall = join $ symbol (GetCC getCont :: Capture (Cont m a) (Plan (App m a) m a))

capture :: forall fs m a.
         (Has (Capture (Cont m a)) fs m)
      => Cont m a -> Plan fs m ()
capture c = symbol (SetCC c ())

instance Pair (Reify k) (Capture k) where
  pair p (Reify klr _) (GetCC kk) = pair p klr kk
  pair p (Reify _ kk) (SetCC k' k)= pair p kk (k',k)

reify :: (Uses (Reify (Cont m a)) is m)
      => Cont m a -> Instruction (Reify (Cont m a)) is m
reify f = Reify (f,pure) (instruction . reify)

test :: Plan (App IO String) IO String
test = do
  lift (putStrLn "Test")
  capture $ Cont $ do
    ln <- lift (putStr "Continue? [y/n]: " *> getLine)
    case ln of
      "y" -> do lift (putStrLn "Continuing...")
                recall
      _   -> return ln
  recall

computer :: (Uses (Reify (Cont m a)) fs m) => Cont m a -> Context fs m -> Instructions fs m
computer c = Instructions . push (reify c)

type App m a = '[Capture (Cont m a)]
newtype Cont m a = Cont { getCont :: Plan (App m a) m a }

main = do
  let t = Cont test :: Cont IO String
  (_,str) <- delta (computer t unsafeBuild') (getCont t)
  putStrLn str
