{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Router
    (Router,routing
    ,route
    ,getRouter,setRouter
    ,getUrl,setUrl
    ,getParam,setParam
    ,getParams
    ,path,sub
    ,page
    ,IsString(..)
    )where

import Ef
import Ef.Type.Nat
import Ef.IO
import Ef.Single

import Document
import Element
import Web
import Ef.Event
import GHCJS.Marshal
import GHCJS.Types
import qualified GHCJS.DOM.History as H
import qualified GHCJS.DOM.EventM as E
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Document as D

import qualified Control.Monad.Trans.Class as Trans

import Data.String
import Unsafe.Coerce

import qualified Data.Map as Map


data Router k
    = forall self super.
      Router { router :: Narrative '[Router] (Narrative self super) ()
             , routerSetter :: Narrative '[Router] (Narrative self super) () -> k
             , routerGetter :: k
             }
    | forall self super. GetRouter (Narrative '[Router] (Narrative self super) () -> k)
    | forall self super. SetRouter (Narrative '[Router] (Narrative self super) ()) k

    | GetUrl (String -> k)
    | SetUrl String k

    | GetParams (Map.Map String String -> k)
    | GetParam String (Maybe String -> k)
    | SetParam String String k

    | forall self super. Path String CacheDuration String (Narrative '[HTML self super] (Narrative self super) ()) k

    | forall self super. Subpath String (Narrative '[Router] (Narrative self super) ()) k
    | forall self super. Dispatch CacheDuration String (Narrative '[HTML self super] (Narrative self super) ())



instance (Monad super)
    => IsString (Narrative '[Router] (Narrative self super) (Maybe String))
    where
        fromString = getParam




routing :: forall self super methods.
           (Monad super, '[Router] <: self, Ma (Methods methods) (Messages self))
        => Use Router methods super
routing = Router rtr routerSet return
    where
        rtr :: Narrative '[Router] (Narrative self super) ()
        rtr = return ()

        routerSet newRtr fs =
            case view fs of
                Router _ rtrs rtrg ->
                    return $ fs .= Router (unsafeCoerce newRtr) rtrs rtrg


-- | Set a routing block as the default router:
--
-- @
--     setRouter $ do
--
--         "user" $ do
--
--             "edit" $ do
--                 ":uid" $ do
--                     uid <- "uid"
--                     editUser uid
--
--             ":uid" $ do
--                 uid <- "uid"
--                 user uid
--
--             def signup
--
--         "/post/:pid" $ do
--             ps <- params
--             post ps
--
--         def index
-- @
--



setRouter :: ('[Router] <: self, Monad super)
          => Narrative '[Router] (Narrative self super) ()
          -> Narrative self super ()
setRouter rtr = self (SetRouter rtr ())



getRouter :: ('[Router] <: self, Monad super)
          => Narrative self super (Narrative '[Router] (Narrative self super) ())
getRouter = self (GetRouter id)



setUrl :: forall self super.
          (Monad super)
       => String -> Narrative '[Router] (Narrative self super) ()
setUrl url = self (SetUrl url () :: Router ())



getUrl :: forall self super.
         (Monad super)
       => Narrative '[Router] (Narrative self super) String
getUrl = self (GetUrl id :: Router String)



getParams :: forall self super.
             (Monad super)
          => Narrative '[Router] (Narrative self super) (Map.Map String String)
getParams = self (GetParams id :: Router (Map.Map String String))



setParam :: forall self super.
            (Monad super)
         => String -> String -> Narrative '[Router] (Narrative self super) ()
setParam p v = self (SetParam p v () :: Router ())



getParam :: forall self super.
            (Monad super)
         => String -> Narrative '[Router] (Narrative self super) (Maybe String)
getParam p = self (GetParam p id :: Router (Maybe String))



path :: forall self super.
        (Monad super)
     => String
     -> CacheDuration
     -> String
     -> Narrative '[HTML self super] (Narrative self super) ()
     -> Narrative '[Router] (Narrative self super) ()
path stencil cchng ttl handler = self (Path stencil cchng ttl handler () :: Router ())



sub :: forall self super.
       (Monad super)
    => String
    -> Narrative '[Router] (Narrative self super) ()
    -> Narrative '[Router] (Narrative self super) ()
sub match handler = self (Subpath match handler () :: Router ())



page :: forall self super.
       (Monad super)
    => CacheDuration
    -> String
    -> Narrative '[HTML self super] (Narrative self super) ()
    -> Narrative '[Router] (Narrative self super) ()
page cchng ttl handler = self (Dispatch cchng ttl handler :: Router ())



instance Ma Router Router where
    ma use Router{..} (GetRouter rtrk)  = use routerGetter (rtrk $ unsafeCoerce router)
    ma use Router{..} (SetRouter rtr k) = use (routerSetter $ unsafeCoerce rtr) k


data BadRoute = BadRoute deriving Show
instance Exception BadRoute


-- | Route a url with the default router.
route :: forall self super.
         ('[Web,Documents,SingleKnot,Router] <: self, Monad super, Lift IO super)
      => String
      -> Narrative self super ()
route ('#':url0) = do
    md <- lookupURL url0
    case md of
        Just doc -> do
            ad <- getActiveDocument
            activateDocument doc
            replaceBody (root ad) (root doc)
            return ()
        Nothing -> do
            router <- getRouter
            doc <- withUrl url0 Map.empty router
            ad <- getActiveDocument
            activateDocument doc
            io $ print "Replacing body content"
            replaceBody (root ad) (root doc)
            io $ print "Replaced"
            return ()
    where

        withUrl :: String
                -> Map.Map String String
                -> Narrative '[Router] (Narrative self super) ()
                -> Narrative self super Document
        withUrl url params rtr = do
            io (print (url,params))
            go rtr
            where

                go :: Narrative '[Router] (Narrative self super) ()
                   -> Narrative self super Document
                go (Return _) = Fail $ toException BadRoute
                go (Super sup) = sup >>= go
                go (Fail e) = Fail e
                go (Say msg cont) =
                    case prj msg of
                        Just x ->
                            case x of
                                GetUrl sk -> go (cont $ sk url)
                                SetUrl nr k -> withUrl nr params (cont k)

                                GetParams psk -> go $ cont $ psk params
                                SetParam p v k -> withUrl url (Map.insert p v params) (cont k)
                                GetParam p mvk -> go $ cont $ mvk (Map.lookup p params)

                                Subpath section more k ->
                                    case match section url of
                                        Just (Left subpath) ->
                                            withUrl subpath params $ unsafeCoerce more
                                        Just (Right ((p,v),subpath)) ->
                                            withUrl subpath (Map.insert p v params) $ unsafeCoerce more
                                        Nothing ->
                                            go (cont k)
                                Path pttrn cchng ttl cntnt k ->
                                    case stencil pttrn url of
                                        Just ps -> do
                                            e <- html Nothing "ef" $ unsafeCoerce cntnt
                                            doc <- getDocument
                                            io $ D.setTitle doc $ Just ttl
                                            return (Document (unsafeCoerce e) ttl url0 params cchng)
                                        Nothing -> go (cont k)

                                Dispatch cchng ttl cntnt -> do
                                    e <- html Nothing "ef" $ unsafeCoerce cntnt
                                    doc <- getDocument
                                    io $ D.setTitle doc $ Just ttl
                                    return (Document (unsafeCoerce e) ttl url0 params cchng)
                         

        match (':':param) ('/':path) =
            let (value,path') = break (== '/') path
            in Just $ Right ((param,value),path')
        match matchPath ('/':path) =
            let (subpath,rest) = splitAt (length matchPath) path
            in case rest of
                   '/':_ -> Just $ Left rest
                   _ -> Nothing
        match _ _ = Nothing

        stencil = withAcc []
            where
                withAcc acc = go
                    where
                        go [] [] = Just acc
                        go ('/':ps) ('/':cs) =
                            let (p,ps') = break (== '/') ps
                                (c,cs') = break (== '/') cs
                            in case p of
                                   (':':pat) -> withAcc ((pat,c):acc) ps' cs'
                                   _ -> if p == c
                                        then go ps' cs'
                                        else Nothing
                        go _ _ = Nothing
