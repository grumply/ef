{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StaticPointers #-}
module Main where



import Ef

import Control.Monad
import Network.Socket
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
            SockAddrUnix "test'"

      main' $
          do
            chan <- awaitOn Local sockAddr
            forever $ receive chan


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
            SockAddrUnix "test'"

      main' $
          do
            chan <- connectTo Local sockAddr
            io (print "Chan created.")
            go chan testSize

  where

    go chan = loop
      where

        loop 0 =
            return ()

        loop n =
            do
              Ef.send chan Ignored Main.simple
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
