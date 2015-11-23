{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Main where



import Ef



main =
    let
      ten_e_5 =
          round 10e5 :: Int

      ten_e_6 =
          round 10e6 :: Int

    in
      do
        a <- runTest

                 main_alternate

                 -- ten_e_5
                 ten_e_6
        print a



{-

All compilations and executions performed with:
    compiler: ghc-7.10.2
    flags: -O2
    processor: 3.4 GHz i7 from late 2012
    memory: 1600 MHz DDR3

Results show that:

  main_thread (10e5) > Time (s) :     Use :   Heap  :      GC : Resident
  ----------------------------------------------------------------------


  main_thread (10e6) > Time (s) :     Use :    Heap :      GC : Resident
  ----------------------------------------------------------------------


-}



data Test n
  where

    Test
        :: (    n
             -> IO Char
           )
        -> Test n



runTest (Test go) n =
    go n



main_alternate
    :: ( Eq n
       , Num n
       )
    => Test n

main_alternate =
    Test (main' . alternates . test)
  where

    test n Alternate{..} =
        parent n
      where

        alternate =
            do
              -- return ()
              atomically $ do
                return ()
              return ()

        parent 0 =
            return 'a'

        parent n =
            do
              alt alternate
              parent (n - 1)
