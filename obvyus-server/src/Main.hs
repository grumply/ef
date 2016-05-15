module Main where

import Ef
import Ef.Fork
import Ef.State
import WebSocket

import qualified Data.Map as Map


client = Proxy :: Proxy 8080

main :: IO ()
main = do
  sock <- makeListenSocket "0.0.0.0" client
  (Object $ state sock *:* state Map.empty *:* Empty) $.
    handleConnections
  return ()

handleConnections = get >>= go
  where
    go sock = do
      ws' <- lift $ do
        (conn,_) <- accept sock
        ws client conn
      forkWith (Object $ ws' *:* Empty) connection id
      go sock

connection = do
  lift $ print "Sending success"
  send client "Success"
  lift $ print "Sent success"
