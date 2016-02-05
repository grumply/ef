{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-
Please take note that this is not expected to demonstrate the ease of writing
coroutines, as it does not, but instead is meant only as a demonstration of the
performance of comparable algorithms. If you can use the standard approach and
don't need the power and modularity of the weave approach, then do so, as it is
vastly more understandable in the simple case.
-}
module Main where



import Ef
import Ef.Knot
import Ef.Knot.Methods

main =
    let
      int =
          10000000 :: Int
      integer =
          10000000 :: Integer
    in
      runTest

        main_weave
        -- main_standard

        int
        -- integer




{-

All compilations and executions performed with:
    compiler: ghc-7.10.2
    flags: -O2
    processor: 3.4 GHz i7 from late 2012
    memory: 1600 MHz

Results show

that, for Int:
  main_weave    : 0.34
  main_standard : 0.006 (- NOINLINE)
  main_standard : 0.19  (+ NOINLINE)

and, for Integer:
  main_weave    : 0.47
  main_standard : 0.14 (- NOINLINE)
  main_standard : 0.27 (+ NOINLINE)

-}



data Test n
  where

    Test
        :: (    n
             -> IO ()
           )
        -> Test n



runTest (Test go) n =
    go n



main_standard
    :: ( Eq n
       , Num n
       )
    => Test n

main_standard =
    Test go
  where

    -- This NOINLINE prevents the compiler spotting the vacuousness in the
    -- machine type case of Int and should compile to a similar algorithm
    -- to that of main_weave - modulo some overhead from Ef.
    {-# NOINLINE go #-}
    go 0 =
        return ()

    go n =
        go' () (go (n - 1))

    go' _ k =
        k



main_weave
    :: ( Eq n
       , Num n
       )
    => Test n

main_weave =
    Test (main' . contained)
  where
    main' program = do
      _ <- (Object $ knots *:* Empty) $. linearize program
      return ()
      
    contained start =
        let p = knotted $ \_ dn -> produce start dn
            c = knotted $ \up _ -> consume (up ())
        in p >-> c
    
    produce start yield =
        go start
      where

        go 0 =
            return ()

        go n =
            do
              yield ()
              go (n - 1)

    consume await =
        go
      where

        go =
            do
              _ <- await
              go
