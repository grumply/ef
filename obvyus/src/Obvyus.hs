{-# language RecordWildCards #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language ExistentialQuantification #-}
module Obvyus where

import Ef
import Ef.Event

import Unsafe.Coerce

data Obvyus k
    = Obvyus
          { _loginSignal :: forall self super. (Signal self super (),k)
          , _setLoginSignal :: forall self super. Signal self super () -> k
          , _logoutSignal :: forall self super. (Signal self super (),k)
          , _setLogoutSignal :: forall self super. Signal self super () -> k
          , _loginStatus :: (Bool,k)
          , _setLoggedOut :: k
          , _setLoggedIn :: k
          }
    | LoginStatus (Bool -> k)
    | forall self super. LoginSignal (Signal self super () -> k)
    | forall self super. SetLoginSignal (Signal self super ()) k
    | forall self super. LogoutSignal (Signal self super () -> k)
    | forall self super. SetLogoutSignal (Signal self super ()) k
    | SetLoggedOut k
    | SetLoggedIn k

loginStatus :: forall self super.
                  (Monad super, '[Obvyus] <: self)
               => Narrative self super Bool
loginStatus = self (LoginStatus id :: Obvyus Bool)

loginSignal :: forall self super.
                  (Monad super, '[Obvyus] <: self)
               => Narrative self super (Signal self super ())
loginSignal = self (LoginSignal id :: Obvyus (Signal self super ()))

setLoginSignal :: forall self super.
                  (Monad super, '[Obvyus] <: self)
               => Signal self super () -> Narrative self super ()
setLoginSignal s = self (SetLoginSignal s () :: Obvyus ())

logoutSignal :: forall self super.
                   (Monad super, '[Obvyus] <: self)
                => Narrative self super (Signal self super ())
logoutSignal = self (LogoutSignal id :: Obvyus (Signal self super ()))

setLogoutSignal :: forall self super.
                   (Monad super, '[Obvyus] <: self)
                => Signal self super () -> Narrative self super ()
setLogoutSignal s = self (SetLogoutSignal s () :: Obvyus ())

setLoggedOut :: forall self super.
                (Monad super, '[Obvyus] <: self)
             => Narrative self super ()
setLoggedOut = self (SetLoggedOut () :: Obvyus ())

setLoggedIn :: forall self super.
               (Monad super, '[Obvyus] <: self)
            => Narrative self super ()
setLoggedIn = self (SetLoggedIn () :: Obvyus ())

instance Ma Obvyus Obvyus where
    ma use Obvyus{..} (LoginSignal sk) = use (snd _loginSignal) (sk $ fst _loginSignal)
    ma use Obvyus{..} (SetLoginSignal s k) = use (_setLoginSignal s) k
    ma use Obvyus{..} (LoginStatus bk) = use (snd _loginStatus) (bk $ fst _loginStatus)
    ma use Obvyus{..} (LogoutSignal sk) = use (snd _logoutSignal) (sk $ fst _logoutSignal)
    ma use Obvyus{..} (SetLogoutSignal s k) = use (_setLogoutSignal s) k
    ma use Obvyus{..} (SetLoggedIn k) = use _setLoggedIn k
    ma use Obvyus{..} (SetLoggedOut k) = use _setLoggedOut k

obvyus :: Use Obvyus methods super
obvyus = Obvyus
    -- Add cookie retrieval and validation to maintain logins across visits.
    { _loginSignal  = (undefined,return)
    , _logoutSignal = (undefined,return)
    , _setLoginSignal = \s fs ->
            let o = view fs
            in return $ fs .= o { _loginSignal = (unsafeCoerce s,snd $ _loginSignal o) }
    , _setLogoutSignal = \s fs ->
            let o = view fs
            in return $ fs .= o { _logoutSignal = (unsafeCoerce s,snd $ _logoutSignal o) }
    , _loginStatus  = (False,return)
    , _setLoggedOut = return
    , _setLoggedIn  = return
    }
