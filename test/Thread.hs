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

                 main_thread

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
      yield          :     0.02 :    1 MB :   72 MB :    0 MB :     0 MB
      fork           :     0.38 :  112 MB :  384 MB :  396 MB :    50 MB
      fork and yield :     0.09 :    1 MB :  584 MB :    0 MB :     0 MB

  main_thread (10e6) > Time (s) :     Use :    Heap :      GC : Resident
  ----------------------------------------------------------------------
      yield          :     0.17 :    1 MB :  720 MB :    0 MB :     0 MB
      fork           :     3.91 : 1349 MB : 3848 MB : 4320 MB :   560 MB
      fork and yield :     0.91 :    1 MB : 5840 MB :    2 MB :     0 MB

These results demonstrate linearity.

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



main_thread
    :: ( Eq n
       , Num n
       )
    => Test n

main_thread =
    Test (main' . threads . test)
  where

    test n Thread{..} =
        go n
      where

        thread =
            do
              yield
              return ()

        go 0 =
            return 'a'

        go n =
            do
              fork thread
              -- yield
              go (n - 1)
