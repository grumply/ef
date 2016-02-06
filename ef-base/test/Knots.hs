{-# LANGUAGE FlexibleContexts #-}
module Main where



import Ef
import Ef.Knot
import Ef.Knot.Methods

import Data.Functor.Identity

main = do
    let
      int =
          10000000 :: Int
      integer =
          10000000 :: Integer

    --  result = runIdentity $ main_weave int--eger
    result <- main_weave int--eger
    print result


{-

for Int:
  0.34

and, for Integer:
  0.47

-}


{-# INLINE main_weave #-}
main_weave :: (Monad m, Eq n, Num n) => n -> m ()
main_weave n = do
    _ <- Object (knot *:* Empty) $. linearize (producer (produce n) >-> consumer consume)
    return ()
  where
    produce start yield =
        go start
      where
        go 0 = return ()
        go n = yield () >> go (n - 1)

    consume await = go where go = await >> go
