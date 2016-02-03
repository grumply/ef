{-# LANGUAGE OverloadedStrings #-}
module Main
    where


import qualified Ef.Runtime.Before as Before

main :: IO ()
main =
    Before.main
