{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StaticPointers #-}
module Main where



import Ef

import Control.Monad
import Network.Socket (SockAddr(..),PortNumber(..),inet_addr)
import System.Environment



main = 
    do
      [mode,size] <- getArgs
      case mode of

         "server" ->
             Main.server port

         "client" ->
             Main.client port (10 ^ read size)



port =
    1028



localhost =
    "127.0.0.1"


{-# INLINE server #-}
server
    :: PortNumber
    -> IO () 

server portNum =
    do
      hostAddress <- inet_addr localhost
      let
        sockAddr = 
            SockAddrUnix "test0"

      main' $
          do
            chan <- awaitOn Local sockAddr
            go chan
  where
  
    go chan = 
        loop
      where
        loop =
            do
              result <- receive chan
              case result of

                  ReceivedClose -> 
                      return ()

                  Invoked _ ->
                      loop

{-# INLINE client #-}
client
    :: PortNumber
    -> Int
    -> IO ()

client portNum testSize =
    do
      hostAddress <- inet_addr localhost
      let
        sockAddr =
            SockAddrUnix "test0"

      main' $
          do
            chan <- connectTo Local sockAddr
            io (print "Chan created.")
            go chan testSize

  where

    go chan = loop
      where

        loop 0 =
            close chan

        loop n =
            do
              Ef.send chan Ignore Main.simple
              loop (n - 1)


{-# INLINE simple #-}
simple
    :: Remote Ef IO ()

simple =
    static method



{-# INLINE method #-}
method
    :: Remoteable Ef IO ()

method =
    remoteable $
        do
          return ()
