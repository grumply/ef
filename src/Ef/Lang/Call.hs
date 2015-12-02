{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeOperators #-}
module Ef.Lang.Call
    ( Remote
    , Remoteable
    , remoteable

    , Channel
    , Remoteness(..)
    , connectTo
    , awaitOn
    , RequestType(Ignore,Awaiting)
    , Compression(..)
    , sendRPC
    , receiveRPC
    , close
    , ReceiveResult(..)
    , runChannel

    , TypeOfScope(..)
    , TypeOfParent(..)
    , CommunicationFailure(..)
    , HandshakeFailure(..)
    ) where
{- This module makes no guarantees of security only
   an attempt at safety via typed channels. Exceptions
   are not propagated across channels; uncaught exceptions
   will propagate up the remote object's scope hierarchy.
   I could embed try inside remoteable, but what would be
   the implications? Does it make sense to recover from
   arbitrary errors and send them back across a channel
   as a String?
-}



import Ef.Core
import Ef.Lang.IO

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Char
import Data.Functor.Identity
import Data.Maybe
import Data.Typeable
import Data.Int
import GHC.Generics
import GHC.StaticPtr
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString.Lazy as NSBL
import qualified Control.Exception as Exc
import System.IO
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BSL
import Codec.Compression.Zlib.Raw
import Unsafe.Coerce



runChannel
    :: ( (Attrs gs) `Witnessing` (Symbol fs)
       , Monad m
       , Lift IO m
       , Typeable fs
       , Typeable m
       )
    => Channel gs m
    -> Pattern fs m (Either CommunicationFailure ReceiveResult)

runChannel chan = try loop
  where

    loop =
        do
          receiveResult <- receiveRPC chan
          case receiveResult of

              ReceivedClose ->
                  return ReceivedClose

              Invoked _ ->
                  loop



data ReceiveResult
  where

    ReceivedClose
        :: ReceiveResult

    Invoked
        :: RequestType
        -> ReceiveResult
        


data BinaryResult =
    forall a.
       Binary a
    => BinaryResult a



data Remoteable variables fs m a =
       ( Typeable fs
       , Typeable m
       , Binary variables
       )
    => Remoteable (BSL.ByteString -> Pattern fs m BinaryResult)



remoteable
    :: ( Typeable fs
       , Typeable m
       , Functor m
       , Binary result
       , Binary variables
       )
    => (    variables
         -> Pattern fs m result  
       )
    -> Remoteable variables fs m result

remoteable method =
    Remoteable $ 
        \bsl ->
            let
              variables =
                  decode bsl 
            in
              fmap BinaryResult (method variables)



type Remote variables fs m a =
    StaticPtr (Remoteable variables fs m a)



data Channel (fs :: [* -> *]) (m :: * -> *)
  where

    Channel
        :: NS.Socket
        -> Channel fs m

undefined_sp =
    static (0 :: Int)



close chan =
    sendRPC chan Close undefined


-- | sendRPC can throw:
--       CouldNotSend
--       BadMessageLength
--       BadMessage
sendRPC
    :: ( Monad m'
       , Typeable fs
       , Lift IO m'
       , Binary a
       , Binary variables
       )
    => Channel fs m
    -> RequestType
    -> variables
    -> Remote variables fs m a
    -> Pattern gs m' a

sendRPC (Channel sock) Close _ _ =
    do
      let
        key =
            staticKey undefined_sp

        request =
            encode (Request Close key BSL.empty)
      
        messageLength =
            BSL.length request

      sendResult <- try $ io (NSBL.send sock request)
      case sendResult of

          Left (_ :: SomeException) ->
              throw CouldNotSend

          Right sentBytes ->
              if sentBytes < messageLength then
                  throw CouldNotSend
              else
                  return (unsafeCoerce ())

sendRPC (Channel sock) requestType variables sp =
    do
      let
        encodedVariables =
            encode variables
            
        key =
            staticKey sp

        request =
            encode (Request requestType key encodedVariables)
      
        messageLength =
            BSL.length request

      sendResult <- try $ io (NSBL.send sock request)
      case sendResult of

          Left (_ :: SomeException) ->
              throw CouldNotSend

          Right sentBytes ->
              if sentBytes < messageLength then
                  throw CouldNotSend
              else
                  case requestType of

                      Ignore ->
                          return (unsafeCoerce ())

                      Awaiting compression ->
                          receive_ sock compression

-- | receiveRPC can throw:
--       BadMessage
--       BadMethod
--       CouldNotReceive
--       CouldNotSend
--   as well as any exceptions the method itself might throw.
receiveRPC
    :: forall fs gs m.
       ( Lift IO m
       , (Attrs gs) `Witnessing` (Symbol fs)
       , Monad m
       , Typeable fs
       , Typeable m
       )
    => Channel gs m
    -> Pattern fs m ReceiveResult

receiveRPC chan@(Channel sock) =
    do
      msg <- try $ io (NSBL.recv sock 17)
      case msg of

          Left (_ :: SomeException) ->
              throw CouldNotReceive

          Right result ->
              case decodeOrFail result of

                  Left _ ->
                      throw BadMessage

                  Right (_,_,Request requestType key encodedVariables) ->
                      let
                        variables =
                            decode encodedVariables

                        execute =
                            call chan variables key

                      in
                        case requestType of

                            Close ->
                                do
                                  io (NS.close sock)
                                  return ReceivedClose

                            Awaiting compression ->
                                do
                                  BinaryResult res <- execute
                                  send_ compression sock result
                                  return (Invoked requestType)

                            Ignore ->
                                do
                                  _ <- execute
                                  return (Invoked requestType)



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

    MessageLength
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

    BadMethod
        :: CommunicationFailure

    CouldNotReceive
        :: CommunicationFailure

    CouldNotSend
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



data Compression
  where

    Compressed
        :: Compression

    Uncompressed
        :: Compression



data RequestType
  where

    Awaiting
        :: Compression
        -> RequestType

    Ignore
        :: RequestType

    Close
        :: RequestType

instance Binary RequestType
  where

    get =
        do
          requestType <- getWord8
          pure $
              case requestType of

                  0 ->
                      Awaiting Compressed

                  1 ->
                      Awaiting Uncompressed

                  2 ->
                      Ignore

                  3 ->
                      Close



    put (Awaiting Compressed) =
        putWord8 0

    put (Awaiting _) =
        putWord8 1

    put Ignore =
        putWord8 2

    put Close =
        putWord8 3



data Request
  where

    Request
        :: RequestType
        -> StaticKey
        -> BSL.ByteString
        -> Request

  deriving Generic

instance Binary Request



data Remoteness
  where

    Local
        :: Remoteness

    Remote
        :: Remoteness

  deriving Eq


receive_
    :: ( Binary a
       , Lift IO m
       , Monad m
       )
    => NS.Socket
    -> Compression
    -> Pattern fs m a

receive_ sock compression =
    do
      lngth <- io (NSBL.recv sock 8)
      MessageLength n <-
          case decodeOrFail lngth of

              Left _ ->
                  throw BadMessageLength

              Right (_,_,a) ->
                  return a

      msg <- io (NSBL.recv sock n)
      case compression of

          Compressed ->
              case decodeOrFail (decompress msg) of

                  Left _ ->
                      throw BadMessage

                  Right (_,_,a) ->
                      return a

          Uncompressed ->
              case decodeOrFail msg of

                  Left _ ->
                      throw BadMessage

                  Right (_,_,a) ->
                      return a



send_
    :: ( Binary a
       , Lift IO m
       , Monad m
       )
    => Compression
    -> NS.Socket
    -> a
    -> Pattern fs m ()

send_ Compressed sock a =
    let
      content =
          compress (encode a)

      contentLength =
          BSL.length content

      encodedLength =
          MessageLength contentLength

      message =
          BSL.append (encode encodedLength) content

    in
      do
        sendResult <- try $ io (NSBL.send sock message)
        case sendResult of

            Left (_ :: SomeException) ->
                throw CouldNotSend

            Right bytesSent ->
                if bytesSent < contentLength + 8 then
                    throw CouldNotSend
                else
                    return ()

send_ _ sock a =
    let
      content =
          encode a

      contentLength =
          BSL.length content

      encodedLength =
          MessageLength contentLength

      message =
          BSL.append (encode encodedLength) content

    in
      do
        sendResult <- try $ io (NSBL.send sock message)
        case sendResult of

            Left (_ :: SomeException) ->
                throw CouldNotSend

            Right bytesSent ->
                if bytesSent < contentLength + 8 then
                    throw CouldNotSend
                else
                    return ()



awaitOn
    :: forall fs gs m.
       ( Typeable gs
       , (Attrs gs) `Witnessing` (Symbol fs)
       , Typeable m
       , Monad m
       , Lift IO m
       )
    => Remoteness
    -> NS.SockAddr
    -> Pattern fs m (Channel gs m)

awaitOn remoteness sockAddr =
    do
      let
        socketFamily =
            if remoteness == Remote then
                NS.AF_INET
            else
                NS.AF_UNIX

      sock <- io $
                  do
                    sock <- NS.socket socketFamily NS.Stream NS.defaultProtocol
                    NS.bind sock sockAddr
                    NS.listen sock 1
                    (sender,_) <- NS.accept sock
                    NS.close sock
                    return sender
      let
        expectedScope =
            TypeOfScope $ (typeOf :: Proxy gs -> TypeRep) (undefined :: Proxy gs)

        expectedParent =
            TypeOfParent $ (typeOf :: Proxy m -> TypeRep) (undefined :: Proxy m)

      Handshake wantedScope wantedParent <- receive_ sock Uncompressed
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
    => Remoteness
    -> NS.SockAddr
    -> Pattern fs m (Channel gs m')

connectTo remoteness sockAddr =
    do
      let
        socketFamily =
            if remoteness == Remote then
                NS.AF_INET
            else
                NS.AF_UNIX

        wantedScope =
            TypeOfScope $ (typeOf :: Proxy gs -> TypeRep) (undefined :: Proxy gs)

        wantedParent =
            TypeOfParent $ (typeOf :: Proxy m' -> TypeRep) (undefined :: Proxy m')

        handshake =
            Handshake wantedScope wantedParent

      sock <- io $
                  do
                    sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
                    NS.connect sock sockAddr
                    return sock

      send_ Uncompressed sock handshake
      handshakeResult <- receive_ sock Uncompressed
      case handshakeResult of

          Denied reason ->
              throw (BadHandshake reason)

          Accepted ->
              return (Channel sock)



call
    :: ( Lift IO m
       , (Attrs gs) `Witnessing` (Symbol fs)
       , Monad m
       , Typeable fs
       , Typeable m
       )
    => Channel gs m
    -> BSL.ByteString
    -> StaticKey
    -> Pattern fs m BinaryResult

call (Channel sock) encodedVariables key =
    do
      possibleMethod <- io $ unsafeLookupStaticPtr key
      case possibleMethod of

          Nothing ->
              throw BadMethod

          Just staticRemoteable ->
              do
                let
                  Remoteable method
                      = deRefStaticPtr staticRemoteable

                method encodedVariables



{-# INLINE receive_ #-}
{-# INLINE receiveRPC #-}
{-# INLINE send_ #-}
{-# INLINE sendRPC #-}
{-# INLINE awaitOn #-}
{-# INLINE remoteable #-}
{-# INLINE call #-}
{-# INLINE close #-}
