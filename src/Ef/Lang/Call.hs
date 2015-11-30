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
    , RequestType(..)
    , Compression(..)
    , send
    , receive
    
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



data Result =
    forall a.
       Binary a
    => Result a



data Remoteable fs m a =
       ( Typeable fs 
       , Typeable m
       )
    => Remoteable (Pattern fs m Result) 



remoteable
    :: ( Typeable fs 
       , Typeable m
       , Functor m
       , Binary a
       )
    => Pattern fs m a -> Remoteable fs m a

remoteable method = 
    Remoteable (fmap Result method)



type Remote fs m a = 
    StaticPtr (Remoteable fs m a)



data Channel (fs :: [* -> *]) (m :: * -> *) 
  where

    Channel
        :: NS.Socket
        -> Channel fs m


-- | send can throw:
--       CouldNotSend
--       BadMessageLength
--       BadMessage 
send
    :: ( Monad m'
       , Typeable fs
       , Lift IO m'
       , Binary a
       )
    => Channel fs m
    -> RequestType
    -> Remote fs m a
    -> Pattern gs m' a

send (Channel sock) requestType sp =
    do
      let
        key =
            staticKey sp
            
        request =
            encode (Request requestType key)

      sendResult <- try $ io (NSBL.send sock request)
      case sendResult of

          Left (_ :: SomeException) ->
              throw CouldNotSend

          Right sentBytes ->
              if sentBytes < 18 then 
                  throw CouldNotSend
              else
                  case requestType of
                  
                      Ignored -> 
                          return (unsafeCoerce ()) 

                      Awaiting compression ->
                          receive_ sock compression

-- | receive can throw: 
--       BadMessage
--       BadMethod 
--       CouldNotReceive
--       CouldNotSend
--   as well as any exceptions the method itself might throw.
receive
    :: forall fs gs m.
       ( Lift IO m
       , (Attrs gs) `Witnessing` (Symbol fs)
       , Monad m 
       , Typeable fs
       , Typeable m
       )
    => Channel gs m
    -> Pattern fs m ()

receive chan@(Channel sock) =
    do
      msg <- try $ io (NSBL.recv sock 18)
      case msg of
          
          Left (_ :: SomeException) -> 
              throw CouldNotReceive

          Right result ->
              case decodeOrFail result of

                  Left _ -> 
                      throw BadMessage 

                  Right (_,_,Request requestType key) ->
                      do
                        Result res <- call chan key
                        case requestType of

                            Awaiting compression -> 
                                send_ compression sock result

                            Ignored ->
                                return ()



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

    Ignored
        :: RequestType

instance Binary RequestType
  where
  
    get =
        do
          requestType <- getWord8
          compression <- getWord8
          case (requestType,compression) of
          
              (0,0) ->
                  return (Awaiting Compressed)
                  
              (0,_) ->
                  return (Awaiting Uncompressed)
                  
              (1,_) ->
                  return Ignored
                  
    

    put (Awaiting Compressed) =
        putWord8 0 >> putWord8 0
        
    put (Awaiting _) =
        putWord8 0 >> putWord8 1

    put Ignored =
        putWord8 1 >> putWord8 0


    
data Request
  where
  
    Request
        :: RequestType
        -> StaticKey
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
    -> StaticKey
    -> Pattern fs m Result

call (Channel sock) key =
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
                      
                method

{-# INLINE receive_ #-}
{-# INLINE receive #-}
{-# INLINE send_ #-}
{-# INLINE send #-}
{-# INLINE awaitOn #-}
{-# INLINE remoteable #-}
{-# INLINE call #-}
