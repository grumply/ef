{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- | (Ef) (O)bject (R)untime (E)nvironment; manager for an object environment that interfaces with GHCi.
     Supports reloading components, extending objects, and other live-develop functionality.
-}

module Ef.Runtime.Before where



import Ef.Core.Narrative
import Ef.Core.Object
import Ef.Core

import Ef.Lang.IO

import Control.Concurrent
import Control.Monad

import Data.IORef
import Data.Maybe

import System.Process
import System.Directory
import System.IO
import System.Timeout


main =
    do
        wd <- getCurrentDirectory
        ghci <- createGHCi wd
        let
            obj = Object $ (before ghci) *:* Empty
        delta obj $
            do
                sendGHCi "\r"
                currentBindings
        return ()



type Stream =
    MVar (Maybe String)



streamTo stream =
    go
    where

        go [] =
            putMVar stream Nothing

        go (x:xs) =
            do
                putMVar stream (Just x)
                go xs


drain
    :: Int
    -> MVar (Maybe String)
    -> IO (Either String String)
drain delay stream =
    do
        first <- takeMVar stream
        case first of

            Nothing ->
                return (Left "")

            Just line ->
                do
                    more <- slowDrain delay stream
                    case more of

                        Left closedResult ->
                            do
                                print "drain.Left"
                                return $ Left $ unlines (line:closedResult)

                        Right moreLines ->
                            do
                                print "drain.Right"
                                return $ Right $ unlines (line:moreLines)



tryDrain delay stream =
    do
        result <- slowDrain 1000000 stream
        case result of

            Left closedResult ->
                do
                    print "tryDrain.Left"
                    return $ Left $ unlines closedResult

            Right moreLines ->
                do
                    print "tryDrain.Right"
                    return $ Right $ unlines moreLines



slowDrain delay stream =
    do
        result <- newIORef []
        continuousDrain result
    where

        continuousDrain result =
            go True
            where

                go isFirst =
                    do
                        attempt <- timeout delay (takeMVar stream)
                        case attempt of

                            Nothing ->
                                (Right . reverse) <$> readIORef result

                            Just Nothing ->
                                (Left . reverse) <$> readIORef result

                            Just (Just line) ->
                                do
                                    modifyIORef result (line:)
                                    go False



data GHCi =
    GHCi
        {
          ghci_in
              :: Handle

        , ghci_out
              :: Stream

        , ghci_err
              :: Stream

        , ghci_process
              :: ProcessHandle

        }

createGHCi wd =
     do
         (Just ghci_in,Just ghci_out_,Just ghci_err_,ghci_process) <-
             createProcess
                (proc "ghci" ["-fobject-code"])
                    { cwd = Just wd
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
         ghci_out <- streamify ghci_out_
         ghci_err <- streamify ghci_err_
         return GHCi{..}
     where

         streamify handle =
             do
                 contents <- lines <$> hGetContents handle
                 stream <- newEmptyMVar
                 forkIO (streamTo stream contents)
                 return stream



data Before k =
    Before
        {
          ghci_
              :: (GHCi,k)

        , killGHCi_
              :: (IO (),k)

        , sendGHCi_
              :: String -> (IO (Either String String,Either String String),k)

        }



before
    :: GHCi
    -> Use Before attrs environment

before GHCi{..} = 
    Before {..}
    where

        ghci_ =
            (GHCi{..},pure)
      
        killGHCi_ =
            (kill,pure)

        kill =
            terminateProcess ghci_process

        sendGHCi_ str =
            (send str,pure)

        send str =
            do
                hPutStrLn ghci_in str
                (,) <$> drain 1000000 ghci_out <*> tryDrain 1000000 ghci_err



data BeforeActions k
    where

        SlowDrainOutput
            :: Int
            -> (IO [String] -> k)
            -> BeforeActions k

        KillGHCi
            :: (IO () -> k)
            -> BeforeActions k

        SendGHCi
            :: String
            -> (IO (Either String String,Either String String) -> k)
            -> BeforeActions k



instance Inflection Before BeforeActions where

    inflect use Before{..} (KillGHCi iok) =
        let
            k = snd killGHCi_ 

            k' = iok (fst killGHCi_)

        in
            use k k'

    inflect use Before{..} (SendGHCi str returns) =
        let
            (result,k) = sendGHCi_ str

            k' = returns result

        in
            use k k'



killGHCi =
    do
        action <- say (KillGHCi id)
        io action



sendGHCi str =
    do
        action <- say (SendGHCi str id)
        io action



currentBindings =
    do
        (bindings,_) <- sendGHCi ":show bindings"
        io (print bindings)
