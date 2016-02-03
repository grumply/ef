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

    , TypeOfAttrs(..)
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
    :: ( (Attrs attrs) `Witnessing` (Symbol scope)
       , Monad parent
       , Lift IO parent
       , Typeable scope
       , Typeable parent
       )
    => Channel attrs parent
    -> Pattern scope parent (Either CommunicationFailure ReceiveResult)

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
        
  deriving Show


data BinaryResult =
    forall a.
       Binary a
    => BinaryResult a



data Remoteable variables scope parent result =
       ( Typeable scope
       , Typeable parent
       , Binary variables
       )
    => Remoteable (BSL.ByteString -> Pattern scope parent BinaryResult)



remoteable
    :: ( Typeable scope
       , Typeable parent
       , Functor parent
       , Binary result
       , Binary variables
       )
    => (    variables
         -> Pattern scope parent result
       )
    -> Remoteable variables scope parent result

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



data Channel (attrs :: [* -> *]) (parent :: * -> *)
  where

    Channel
        :: NS.Socket
        -> Channel attrs parent

undefined_sp =
    static (0 :: Int)



close chan =
    sendRPC chan Close undefined


-- | sendRPC can throw:
--       CouldNotSend
--       BadMessageLength
--       BadMessage
sendRPC
    :: ( Monad parent'
       , Typeable scope
       , Lift IO parent'
       , Binary result
       , Binary variables
       , (Attrs attrs) `Witnessing` (Symbol scope)
       )
    => Channel attrs parent
    -> RequestType
    -> variables
    -> Remote variables scope parent result
    -> Pattern scope' parent' result

sendRPC (Channel sock) Close _ _ =
    do
      let
        key =
            staticKey undefined_sp

        encodedRequest =
            encode (Request Close key BSL.empty)
      
        messageLength =
            BSL.length encodedRequest

        encodedMessageLength =
            encode (MessageLength messageLength)

      messageLengthSendResult <- try $ io (NSBL.send sock encodedMessageLength)
      case messageLengthSendResult of
        
          Left (_ :: SomeException) -> 
              throw CouldNotSend
              
          Right sentBytes ->
              when (sentBytes < 8) (throw CouldNotSend)

      sendResult <- try $ io (NSBL.send sock encodedRequest)
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

        encodedRequest =
            encode (Request requestType key encodedVariables)
      
        messageLength =
            BSL.length encodedRequest

        encodedMessageLength =
            encode (MessageLength messageLength)

      messageLengthSendResult <- try $ io (NSBL.send sock encodedMessageLength)
      case messageLengthSendResult of
        
          Left (_ :: SomeException) -> 
              throw CouldNotSend
              
          Right bytesSent ->
              when (bytesSent < 8) (throw CouldNotSend)

      sendResult <- try $ io (NSBL.send sock encodedRequest)
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
    :: forall scope attrs parent.
       ( Lift IO parent
       , (Attrs attrs) `Witnessing` (Symbol scope)
       , Monad parent
       , Typeable scope
       , Typeable parent
       )
    => Channel attrs parent
    -> Pattern scope parent ReceiveResult

receiveRPC chan@(Channel sock) =
    do
      lengthMsg <- try $ io (NSBL.recv sock 8)
      msgLength <- 
          case lengthMsg of

              Left (_ :: SomeException) -> 
                  throw CouldNotReceive

              Right mlength ->
                  case decodeOrFail mlength of

                      Left _ -> 
                          throw BadMessageLength

                      Right (_,_,MessageLength lngth) ->
                          return lngth

      msg <- try $ io (NSBL.recv sock msgLength)
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



newtype TypeOfAttrs =
    TypeOfAttrs TypeRep
  deriving (Eq,Show,Generic)

instance Binary TypeOfAttrs



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
        :: (TypeOfAttrs,TypeOfAttrs)
        -> (TypeOfParent,TypeOfParent)
        -> HandshakeFailure

    BadScope
        :: (TypeOfAttrs,TypeOfAttrs)
        -> HandshakeFailure

    BadParent
        :: (TypeOfParent,TypeOfParent)
        -> HandshakeFailure

  deriving (Generic,Show)

instance Binary HandshakeFailure



data Handshake
  where

    Handshake
        :: TypeOfAttrs
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

  deriving Show


data RequestType
  where

    Awaiting
        :: Compression
        -> RequestType

    Ignore
        :: RequestType

    Close
        :: RequestType
  
  deriving Show

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
    :: ( Binary result
       , Lift IO parent
       , Monad parent
       )
    => NS.Socket
    -> Compression
    -> Pattern scope parent result

receive_ sock compression =
    do
      lngth <- io (NSBL.recv sock 8)
      messageLength <-
          case decodeOrFail lngth of

              Left _ ->
                  throw BadMessageLength

              Right (_,_,MessageLength lngth) ->
                  return lngth

      msg <- io (NSBL.recv sock messageLength)
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
    :: ( Binary result
       , Lift IO parent
       , Monad parent
       )
    => Compression
    -> NS.Socket
    -> result
    -> Pattern scope parent ()

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
    :: forall scope attrs parent.
       ( Typeable attrs
       , (Attrs attrs) `Witnessing` (Symbol scope)
       , Typeable parent
       , Monad parent
       , Lift IO parent
       )
    => Remoteness
    -> NS.SockAddr
    -> Pattern scope parent (Channel attrs parent)

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
        expectedAttrs =
            TypeOfAttrs $ typeOf (undefined :: Proxy attrs)

        expectedParent =
            TypeOfParent $ typeOf (undefined :: Proxy parent)

      Handshake wantedAttrs wantedParent <- receive_ sock Uncompressed
      let
        compareAttrs =
            expectedAttrs == wantedAttrs

        compareParent =
            expectedParent == wantedParent

      case (compareAttrs, compareParent) of

          (False,False) ->
              do
                let
                  failure =
                      BadScopeAndParent
                          (expectedAttrs,wantedAttrs)
                          (expectedParent,wantedParent)

                send_ Uncompressed sock (Denied failure)
                io (NS.close sock)
                throw (BadHandshake failure)

          (False,_) ->
              do
                let
                  failure =
                      BadScope (expectedAttrs,wantedAttrs)

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
    :: forall scope attrs parent parent'.
       ( Typeable attrs
       , Typeable parent'
       , Lift IO parent
       , Monad parent
       )
    => Remoteness
    -> NS.SockAddr
    -> Pattern scope parent (Channel attrs parent')

connectTo remoteness sockAddr =
    do
      let
        socketFamily =
            if remoteness == Remote then
                NS.AF_INET
            else
                NS.AF_UNIX

        wantedAttrs =
            TypeOfAttrs $ typeOf (undefined :: Proxy attrs)

        wantedParent =
            TypeOfParent $ typeOf (undefined :: Proxy parent')

        handshake =
            Handshake wantedAttrs wantedParent

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
    :: ( Lift IO parent
       , (Attrs attrs) `Witnessing` (Symbol scope)
       , Monad parent
       , Typeable scope
       , Typeable parent
       )
    => Channel attrs parent
    -> BSL.ByteString
    -> StaticKey
    -> Pattern scope parent BinaryResult

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
