{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Lang.Scoped.Call where



import Ef.Core
import Ef.Lang.IO

import Control.Monad
import Data.Binary
import Data.Char
import Data.Maybe
import Data.Typeable
import Data.Int
import GHC.Generics
import GHC.StaticPtr
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString.Lazy as NSBL
import System.IO
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BSL
import Codec.Compression.Zlib.Raw



data Calling k
  where

    Call
        :: StaticKey
        -> (a -> k)
        -> Calling k



instance Binary (Calling k)
  where

    get =
        do
          sk <- get
          return (Call sk id)



    put (Call sk _) =
        put sk



data Channel (fs :: [* -> *]) (m :: * -> *) 
  where

    Channel
        :: NS.Socket
        -> Channel fs m



-- dispatch
--     :: ( Monad m'
--        , Typeable fs) Channel fs m
--     -> StaticPtr (Pattern fs m ())
--     -> Pattern gs m' ()

-- dispatch (Channel sock) sp =
--     do
--       io $
--           do
--             let
--               key =
--                   staticKey sp

--             undefined




newtype TypeOfScope =
    TypeOfScope TypeRep
  deriving (Eq,Show,Generic)

instance Binary TypeOfScope



newtype TypeOfParent =
    TypeOfParent TypeRep
  deriving (Eq,Show,Generic)

instance Binary TypeOfParent




data MessageLength
  where

    CompressedMessageLength
        :: Int64
        -> MessageLength

    UncompressedMessageLength
        :: Int64
        -> MessageLength
  deriving Generic

instance Binary MessageLength




data CommunicationFailure
  where

    BadHandshake
        :: HandshakeFailure
        -> CommunicationFailure

    BadMessageLength
        :: CommunicationFailure

    BadMessage
        :: CommunicationFailure

  deriving Show

instance Exception CommunicationFailure



data HandshakeFailure
  where

    BadScopeAndParent
        :: (TypeOfScope,TypeOfScope)
        -> (TypeOfParent,TypeOfParent)
        -> HandshakeFailure

    BadScope
        :: (TypeOfScope,TypeOfScope)
        -> HandshakeFailure

    BadParent
        :: (TypeOfParent,TypeOfParent)
        -> HandshakeFailure

  deriving (Generic,Show)

instance Binary HandshakeFailure



data Handshake
  where

    Handshake
        :: TypeOfScope
        -> TypeOfParent
        -> Handshake

    Accepted
        :: Handshake

    Denied
        :: HandshakeFailure
        -> Handshake

  deriving Generic

instance Binary Handshake



receive_
    :: ( Binary a
       , Lift IO m
       , Monad m
       )
    => NS.Socket
    -> Pattern fs m a

receive_ sock =
    do
      lngth <- io (NSBL.recv sock 9)
      messageLength <-
          case decodeOrFail lngth of

              Left _ ->
                  throw BadMessageLength

              Right (_,_,a) ->
                  return a

      case messageLength of

          CompressedMessageLength n ->
              do
                msg <- io (NSBL.recv sock n)
                case decodeOrFail (decompress msg) of

                    Left _ ->
                        throw BadMessage

                    Right (_,_,a) ->
                        return a

          UncompressedMessageLength n ->
              do
                msg <- io (NSBL.recv sock n)
                case decodeOrFail msg of

                    Left _ ->
                        throw BadMessage

                    Right (_,_,a) ->
                        return a



data Compression = Compressed | Uncompressed

send_
    :: ( Binary a
       , Lift IO m
       , Monad m
       )
    => Compression
    -> NS.Socket
    -> a
    -> Pattern fs m Int64

send_ Compressed sock a =
    let
      content =
          compress (encode a)

      contentLength =
          CompressedMessageLength (BSL.length content)

      message =
          BSL.append (encode contentLength) content

    in
      io (NSBL.send sock message)

send_ _ sock a =
    let
      content =
          encode a

      contentLength =
          UncompressedMessageLength (BSL.length content)

      message =
          BSL.append (encode contentLength) content

    in
      io (NSBL.send sock message)



awaitOn
    :: forall fs gs m m'.
       ( Typeable gs
       , Typeable m'
       , Monad m
       , Lift IO m
       )
    => NS.SockAddr -> Pattern fs m (Channel gs m')

awaitOn sockAddr =
    do
      sock <- io $
                  do
                    sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
                    NS.bind sock sockAddr
                    NS.listen sock 1
                    (sender,_) <- NS.accept sock
                    NS.close sock
                    return sender
      let
        expectedScope =
            TypeOfScope $ (typeOf :: Proxy gs -> TypeRep) (undefined :: Proxy gs)

        expectedParent =
            TypeOfParent $ (typeOf :: Proxy m' -> TypeRep) (undefined :: Proxy m')

      Handshake wantedScope wantedParent <- receive_ sock
      let
        compareScope =
            expectedScope == wantedScope

        compareParent =
            expectedParent == wantedParent

      case (compareScope, compareParent) of

          (False,False) ->
              do
                let
                  failure =
                      BadScopeAndParent
                          (expectedScope,wantedScope)
                          (expectedParent,wantedParent)

                send_ Uncompressed sock (Denied failure)
                io (NS.close sock)
                throw (BadHandshake failure)

          (False,_) ->
              do
                let
                  failure =
                      BadScope (expectedScope,wantedScope)

                send_ Uncompressed sock (Denied failure)
                io (NS.close sock)
                throw (BadHandshake failure)

          (_,False) ->
              do
                let
                  failure =
                      BadParent (expectedParent,wantedParent)

                send_ Uncompressed sock (Denied failure)
                io (NS.close sock)
                throw (BadHandshake failure)

          (_,_) ->
              do
                send_ Uncompressed sock Accepted
                return (Channel sock)



connectTo
    :: forall fs gs m m'.
       ( Typeable gs
       , Typeable m'
       , Lift IO m
       , Monad m
       )
    => NS.SockAddr -> Pattern fs m (Channel gs m')

connectTo sockAddr =
    do
      let
        wantedScope =
            TypeOfScope $ (typeOf :: Proxy gs -> TypeRep) (undefined :: Proxy gs)

        wantedParent =
            TypeOfParent $ (typeOf :: Proxy m' -> TypeRep) (undefined :: Proxy m')

        handshake =
            Handshake wantedScope wantedParent

      sock <- io $
                  do
                    sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
                    NS.connect sock sockAddr
                    return sock

      send_ Compressed sock handshake
      handshakeResult <- receive_ sock
      case handshakeResult of

          Denied reason ->
              throw (BadHandshake reason)

          Accepted ->
              return (Channel sock)







call_
    :: Is Calling fs m
    => StaticKey
    -> Pattern fs m a

call_ sk =
    join $ self (Call sk id)


class Remotable a
  where

    remote
        :: Handle
        -> StaticPtr a
        -> Pattern fs m ()


-- remote
--     :: Handle
--     -> StaticPtr (Pattern fs m a)
--     -> Pattern gs m' ()

-- remote h sp =
--     let
--       encoded =
--           encode (staticKey sp)

--     in
--       do
--         BSL.hPut h encoded



data Callable k
  where

    Callable
        :: k
        -> Callable k



instance Uses Callable gs m
    => Binary (Attribute Callable gs m)
  where

    get =
        return caller



    put _ =
        pure ()



caller
    :: Uses Callable gs m
    => Attribute Callable gs m

caller =
    Callable pure



instance Callable `Witnessing` Calling
  where

    witness use (Callable k) (Call sk ak) =
        let
          sp =
              unsafePerformIO (unsafeLookupStaticPtr sk)

          a =
              deRefStaticPtr (fromJust sp)

        in
          sp `seq` use k (ak a)
