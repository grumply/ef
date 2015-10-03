{-# LANGUAGE DataKinds #-}
module Effect.Capture where

import Mop

data Capture k' k
  = Checkpoint (k' -> k)
  | Recall k' k
data Reify k' k = Reify (k',k) (k' -> k)

newtype Cont m a = Cont { recall :: Plan '[Capture (Cont m a)] m a }

getCC :: (Has (Capture (Cont m a)) fs m)
           => Plan fs m (Cont m a)
getCC = symbol (Checkpoint id)

instance Pair (Reify k) (Capture k) where
  pair p (Reify kl _) (Checkpoint k) = pair p kl k
  pair p (Reify _ kr) (Recall k' k) = pair p kr (k',k)

reify :: (Uses (Reify (Cont m a)) is m)
      => Cont m a -> Instruction (Reify (Cont m a)) is m
reify f = Reify (f,pure) (instruction . reify)

cont :: Monad m => Cont m a -> m (Instructions '[Reify (Cont m a)] m,a)
cont c = delta (Instructions $ reify c *:* Empty) (recall c)

test :: Cont IO ()
test = Cont $ do
  lift (putStrLn "Test")
  here :: Cont IO () <- getCC
  ln <- lift (putStr "Continue? [y/n]: " *> getLine)
  case ln of
    "y" -> do
       lift (putStrLn "Continuing...")
       recall here
    _   -> return ()


main = do
  (_,_) <- cont test
  print ()
