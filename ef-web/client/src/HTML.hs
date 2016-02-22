{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module HTML
    ( Element(..), Blocking(..), Client(..)
    , SomeEvent(..), Event(..)
    , React.event, React.Signal
    , html, with, (>:), child, listen, intercept 
    , (=:), (+#), (-#), (-:)
    , getScreen, getWindow, getDrawer, getDocument
    , Element.getBody, Element.setBody, Element.appendBody
    , client, simpleClient
    , Element.HTML
    , Element.on
    , Element.addClass, Element.setClass, Element.setAttr, Element.setText
    , Element.removeStyle
    , resizeSignal, scrollSignal
    , getWindowWidth
    , element
    , reactiveDependentFeature
    , Web.query, Web.write, arrive
    , module Data.Promise
    ) where

import Ef
import Ef.IO
import Ef.Single hiding (Client)
import Ef.Fiber

import Data.Promise

import qualified Ef.Event as React
import qualified Ef.Knot.Methods.Single as Method
import qualified Ef.Fiber.Methods as Method

import Element
import Event
import Signaled
import Queue
import Web
import qualified Web.Methods as Method

import qualified Control.Monad.Trans.Class as Trans

import Control.Monad
import Unsafe.Coerce

data Blocking = Blocking | Nonblocking deriving Eq

simpleClient f = client $ Client (\x -> return (x,())) (const f) go
    where
        go intcptr _ = start
            where
                start obj = do
                    (obj',_) <- obj $. intcptr Blocking
                    start obj'

data Client self methods super primeResult buildResult = Client
    { prime :: Methods '[Method.Web,Method.SingleKnot] (Implementation methods super)
                -> super (Methods methods (Implementation methods super),primeResult)
    , build :: primeResult -> Narrative self super buildResult
    , drive :: (Blocking -> Narrative self super ()) -> buildResult -> Object methods super -> super ()
    }

client :: ( Ma (Methods methods) (Messages self)
          , Subclass '[Method.Web,Method.SingleKnot] methods
          , '[Web,SingleKnot] <: self
          , Lift IO super
          , Monad super
          )
       => Client self methods super primeResult buildResult -> super ()
client Client{..} = do
    env <- lift Method.createWeb
    let root = env *:* Method.singleKnot *:* Empty
    (obj,a) <- prime root
    (obj',b) <- Object obj $. build a
    (_,buf) <- obj' $. do
        Signaled buf <- getSignaled
        return $ unsafeCoerce buf
    drive (intercepter buf) b obj'
    where
       intercepter globalBuffer = go
           where
               go b = do
                   when (b == Nonblocking) (lift $ arrive globalBuffer ([],undefined))
                   evss <- lift (collect globalBuffer)
                   React.event $ \event ->
                       forM_ evss $ \(ev,s) -> forM_ ev (React.trigger event s)

