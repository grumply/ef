{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where



import Ef

import Control.Monad
import Network.Socket
import System.Environment

import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get
import Data.Typeable



main =
    do
      [mode,size] <- getArgs
      sockAddr <- unixSocketAddress
      -- sockAddr <- inetSocketAddress
      case mode of

         "server" ->
             Main.server sockAddr

         "client" ->
             Main.client sockAddr (10 ^ read size)



inetSocketAddress
    :: IO SockAddr

inetSocketAddress =
    do
      hostAddress <- inet_addr "127.0.0.1"
      return $ SockAddrInet 1028 hostAddress



unixSocketAddress
    :: IO SockAddr

unixSocketAddress =
    return $ SockAddrUnix "test5"



{-# INLINE server #-}
server
    :: SockAddr
    -> IO ()

server sockAddr =
    do
      delta (Object Empty) $
          do
            chan <- awaitOn Local sockAddr
            result <- try $ runChannel chan
            io $  
                case result of

                    Left (e :: SomeException) -> 
                        print $ e

                    Right r ->
                        print $ r

      return ()



{-# INLINE client #-}
client
    :: SockAddr
    -> Int
    -> IO ()

client sockAddr testSize =
    do
      delta (Object Empty) $
          do
            chan <- connectTo Local sockAddr
            io (print "Chan created.")
            go chan testSize
      return ()

  where

    go
        :: Channel '[] IO
        -> Int
        -> Pattern '[] IO ()

    go chan = loop
      where

        loop
            :: Int
            -> Pattern '[] IO ()

        loop 0 =
            return ()

        loop n =
            do
              Ef.sendRPC chan Ignore () Main.simple
              loop (n - 1)



{-# INLINE simple #-}
simple
    :: Remote () '[] IO ()

simple =
    static method



{-# INLINE method #-}
method
    :: Remoteable () '[] IO ()

method =
    remoteable $
        \() ->
            return ()
