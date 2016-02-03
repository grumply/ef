{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where


import Ef.Core

import Ef.Lang.Scoped.Fiber
import Ef.Lang.IO


main =
    let
      ten_e_5 =
          round 10e5 :: Int

      ten_e_6 =
          round 10e6 :: Int

    in
      do
        str <- runTest
                   main_fiber

                   -- (round 10e5)
                   ten_e_5
                   -- ten_e_6
        putStrLn str



data Test input output
  where

    Test
        :: (    input
             -> IO output
           )
        -> Test input output



runTest :: Test input output -> input -> IO output
runTest (Test go) n =
    go n



main_fiber
    :: Test Int String

main_fiber =
    let
        obj =
            Object (fiberer *:* Empty)
    in
        Test (fmap snd . delta obj . fibers . test)
  where

    test 
        :: Int
        -> Fiber '[Fibering] IO
        -> Pattern '[Fibering] IO String

    test n Fiber{..} =
        focus $ go n
      where

        fiber =
            do
              yield
              return ()

        go 0 =
            return "Success"

        go n =
            do
              fork (const fiber)
              yield
              go (n - 1)
