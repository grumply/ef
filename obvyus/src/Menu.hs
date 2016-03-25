{-# language RecordWildCards #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
{-# language NoMonomorphismRestriction #-}
module Menu where

import Ef
import Ef.Event

import Lotus

import qualified GHCJS.DOM.Types as T

import Unsafe.Coerce

{-
   Note the calls to unsafeCoerce in `menu`. These are necessary to fulfill the
   `forall self super.` but are considered safe because their use is protected
   by correct invocations/use. Trying to put the `self` and `super` inside the
   type of `Menu` will cause a recursive type exception when composing objects
   that use an implementation of `menu`. It would be acceptable to add `self`
   and `super` to menu if they referred to a supertype instead of a self type,
   in which case the ultimate invocation type would be, for instance,
   `Narrative '[Menu self super] (Narrative self super) x` as can be seen in
   HTML, Lavender, etc....
-}

data HomeButton k
    = HomeButton
          { _homeNode :: (Node,k)
          , _setHomeNode :: Node -> k
          , _homeClicks :: forall self super. (Signal self super T.MouseEvent,k)
          , _setHomeClicks :: forall self super. Signal self super T.MouseEvent -> k
          }

data ProvacativeButton k
    = ProvacativeButton
          { _provacativeNode :: (Node,k)
          , _setProvacativeNode :: Node -> k
          , _provacativeClicks :: forall self super. (Signal self super T.MouseEvent,k)
          , _setProvacativeClicks :: forall self super. Signal self super T.MouseEvent -> k
          }

data InterestingButton k
    = InterestingButton
          { _interestingNode :: (Node,k)
          , _setInterestingNode :: Node -> k
          , _interestingClicks :: forall self super. (Signal self super T.MouseEvent,k)
          , _setInterestingClicks :: forall self super. Signal self super T.MouseEvent -> k
          }

data LoginWidget k
    = LoginWidget
          { _displayingLoggedIn :: (Bool,k)
          , _displayLoggedIn :: k
          , _displayNotLoggedIn :: k
          , _loginWidgetNode :: (Node,k)
          , _loginClicks :: forall self super. (Signal self super T.MouseEvent,k)
          , _setLoginClicks :: forall self super. Signal self super T.MouseEvent -> k
          , _setLoginWidgetNode :: Node -> k
          , _loggedInWidgetNode :: (Node,k)
          , _setLoggedInWidgetNode :: Node -> k
          }

data Menu k
    = Menu
          { _menuNode          :: (Node,k)
          , _setMenuNode       :: Node -> k
          , _homeButton        :: HomeButton k
          , _provacativeButton :: ProvacativeButton k
          , _interestingButton :: InterestingButton k
          , _loginWidget       :: LoginWidget k
          }
    | MenuNode (Node -> k)
    | SetMenuNode Node k

    | HomeNode (Node -> k)
    | SetHomeNode Node k
    | forall self super. HomeClicks (Signal self super T.MouseEvent -> k)
    | forall self super. SetHomeClicks (Signal self super T.MouseEvent) k

    | ProvacativeNode (Node -> k)
    | SetProvacativeNode Node k
    | forall self super. ProvacativeClicks (Signal self super T.MouseEvent -> k)
    | forall self super. SetProvacativeClicks (Signal self super T.MouseEvent) k

    | InterestingNode (Node -> k)
    | SetInterestingNode Node k
    | forall self super. InterestingClicks (Signal self super T.MouseEvent -> k)
    | forall self super. SetInterestingClicks (Signal self super T.MouseEvent) k

    | DisplayingLoggedIn (Bool -> k)
    | DisplayLoggedIn k
    | DisplayNotLoggedIn k
    | forall self super. LoginClicks (Signal self super T.MouseEvent -> k)
    | forall self super. SetLoginClicks (Signal self super T.MouseEvent) k
    | LoginWidgetNode (Node -> k)
    | SetLoginWidgetNode Node k
    | LoggedInWidgetNode (Node -> k)
    | SetLoggedInWidgetNode Node k


menuNode = self (MenuNode id)

setMenuNode n = self (SetMenuNode n ())


homeNode = self (HomeNode id)

setHomeNode n = self (SetHomeNode n ())

homeClicks :: (Monad super, '[Menu] <: self)
           => Narrative self super (Signal self super T.MouseEvent)
homeClicks = self (HomeClicks id)

setHomeClicks :: (Monad super, '[Menu] <: self)
              => Signal self super T.MouseEvent -> Narrative self super ()
setHomeClicks hc = self (SetHomeClicks hc ())


provacativeNode = self (ProvacativeNode id)

setProvacativeNode n = self (SetProvacativeNode n ())

provacativeClicks :: (Monad super, '[Menu] <: self)
                  => Narrative self super (Signal self super T.MouseEvent)
provacativeClicks = self (ProvacativeClicks id)

setProvacativeClicks :: (Monad super, '[Menu] <: self)
                     => Signal self super T.MouseEvent -> Narrative self super ()
setProvacativeClicks pc = self (SetProvacativeClicks pc ())


interestingNode = self (InterestingNode id)

setInterestingNode n = self (SetInterestingNode n ())

interestingClicks :: (Monad super, '[Menu] <: self)
                  => Narrative self super (Signal self super T.MouseEvent)
interestingClicks = self (InterestingClicks id)

setInterestingClicks :: (Monad super, '[Menu] <: self)
                     => Signal self super T.MouseEvent -> Narrative self super ()
setInterestingClicks c = self (SetInterestingClicks c ())


displayingLoggedIn = self (DisplayingLoggedIn id)

displayLoggedIn = self (DisplayLoggedIn ())

displayNotLoggedIn = self (DisplayNotLoggedIn ())

loginClicks :: (Monad super, '[Menu] <: self)
            => Narrative self super (Signal self super T.MouseEvent)
loginClicks = self (LoginClicks id)

setLoginClicks :: (Monad super ,'[Menu] <: self)
               => Signal self super T.MouseEvent -> Narrative self super ()
setLoginClicks c = self (SetLoginClicks c ())

loginWidgetNode = self (LoginWidgetNode id)

setLoginWidgetNode n = self (SetLoginWidgetNode n ())

loggedInWidgetNode = self (LoggedInWidgetNode id)

setLoggedInWidgetNode n = self (SetLoggedInWidgetNode n ())


instance Ma (Menu) (Menu) where
    ma use Menu{..} (MenuNode nk) =
        use (snd _menuNode) (nk $ fst _menuNode)
    ma use Menu{..} (SetMenuNode mn k) =
        use (_setMenuNode mn) k

    ma use Menu{..} (HomeNode nk) =
        use (snd $ _homeNode _homeButton)
            (nk $ fst $ _homeNode _homeButton)
    ma use Menu{..} (SetHomeNode n k) =
        use (_setHomeNode _homeButton $ n) k
    ma use Menu{..} (HomeClicks sk) =
        use (snd $ _homeClicks _homeButton)
            (sk $ fst $ _homeClicks _homeButton)
    ma use Menu{..} (SetHomeClicks hc k) =
        use (_setHomeClicks _homeButton $ hc) k

    ma use Menu{..} (ProvacativeNode nk) =
        use (snd $ _provacativeNode _provacativeButton)
            (nk $ fst $ _provacativeNode _provacativeButton)
    ma use Menu{..} (SetProvacativeNode n k) =
        use (_setProvacativeNode _provacativeButton $ n) k
    ma use Menu{..} (ProvacativeClicks sk) =
        use (snd $ _provacativeClicks _provacativeButton)
            (sk $ fst $ _provacativeClicks _provacativeButton)
    ma use Menu{..} (SetProvacativeClicks sc k) =
        use (_setProvacativeClicks _provacativeButton $ sc) k

    ma use Menu{..} (InterestingNode nk) =
        use (snd $ _interestingNode _interestingButton)
            (nk $ fst $ _interestingNode _interestingButton)
    ma use Menu{..} (SetInterestingNode n k) =
        use (_setInterestingNode _interestingButton $ n) k
    ma use Menu{..} (InterestingClicks sk) =
        use (snd $ _interestingNode _interestingButton)
            (sk $ fst $ _interestingClicks _interestingButton)
    ma use Menu{..} (SetInterestingClicks ic k) =
        use (_setInterestingClicks _interestingButton $ ic) k

    ma use Menu{..} (DisplayingLoggedIn bk) =
        use (snd $ _displayingLoggedIn _loginWidget)
            (bk $ fst $ _displayingLoggedIn _loginWidget)
    ma use Menu{..} (DisplayLoggedIn k) =
        use (_displayLoggedIn _loginWidget) k
    ma use Menu{..} (DisplayNotLoggedIn k) =
        use (_displayNotLoggedIn _loginWidget) k
    ma use Menu{..} (LoginClicks lk) =
        use (snd $ _loginClicks _loginWidget)
            (lk $ fst $ _loginClicks _loginWidget)
    ma use Menu{..} (SetLoginClicks lc k) =
        use (_setLoginClicks _loginWidget $ lc) k
    ma use Menu{..} (LoginWidgetNode nk) =
        use (snd $ _loginWidgetNode _loginWidget)
            (nk $ fst $ _loginWidgetNode _loginWidget)
    ma use Menu{..} (SetLoginWidgetNode n k) =
        use (_setLoginWidgetNode _loginWidget $ n) k
    ma use Menu{..} (LoggedInWidgetNode nk) =
        use (snd $ _loggedInWidgetNode _loginWidget)
            (nk $ fst $ _loggedInWidgetNode _loginWidget)
    ma use Menu{..} (SetLoggedInWidgetNode n k) =
        use (_setLoggedInWidgetNode _loginWidget $ n) k

menu :: forall self super methods.
        (Ma (Methods methods) (Messages self), Monad super, '[Menu] <: self, Subclass '[Menu] methods)
     => Menu (Implementation methods super)
menu = Menu
    { _menuNode = (undefined,return)
    , _setMenuNode = \mn fs ->
          let m = view fs
          in return $ fs .= m { _menuNode = (mn,snd $ _menuNode m) }

    , _homeButton = HomeButton
            { _homeNode = (undefined,return)
            , _setHomeNode = \hn fs ->
                    let m = view fs
                        hb = _homeButton m
                    in return $ fs .= m { _homeButton = hb { _homeNode = (hn, snd $ _homeNode hb) } }
            , _homeClicks = (undefined,return)
            , _setHomeClicks = \hc fs ->
                    let m = view fs
                        hb = _homeButton m
                    in return $ fs .= m { _homeButton = hb { _homeClicks = (unsafeCoerce hc, snd $ _homeClicks hb) } }
            }

    , _provacativeButton = ProvacativeButton
            { _provacativeNode = (undefined,return)
            , _setProvacativeNode = \pn fs ->
                    let m = view fs
                        pb = _provacativeButton m
                    in return $ fs .= m { _provacativeButton = pb { _provacativeNode = (pn, snd $ _provacativeNode pb) } }
            , _provacativeClicks = (undefined,return)
            , _setProvacativeClicks = \pc fs ->
                    let m = view fs
                        pb = _provacativeButton m
                    in return $ fs .= m { _provacativeButton = pb { _provacativeClicks = (unsafeCoerce pc, snd $ _provacativeClicks pb) } }
            }

    , _interestingButton = InterestingButton
            { _interestingNode = (undefined,return)
            , _setInterestingNode = \ind fs ->
                    let m = view fs
                        ib = _interestingButton m
                    in return $ fs .= m { _interestingButton = ib { _interestingNode = (ind, snd $ _interestingNode ib) } }
            , _interestingClicks = (undefined,return)
            , _setInterestingClicks = \ic fs ->
                    let m = view fs
                        ib = _interestingButton m
                    in return $ fs .= m { _interestingButton = ib { _interestingClicks = (unsafeCoerce ic, snd $ _interestingClicks ib) } }
            }

    , _loginWidget = LoginWidget
            { _displayingLoggedIn = (False,return)
            , _displayLoggedIn = return
            , _displayNotLoggedIn = return
            , _loginClicks = (undefined,return)
            , _setLoginClicks = \lc fs ->
                    let m = view fs
                        lw = _loginWidget m
                    in return $ fs .= m { _loginWidget = lw { _loginClicks = (unsafeCoerce lc, snd $ _loginClicks lw) } }
            , _loginWidgetNode = (undefined,return)
            , _setLoginWidgetNode = \n fs ->
                    let m = view fs
                        lw = _loginWidget m
                    in return $ fs .= m { _loginWidget = lw { _loginWidgetNode = (n, snd $ _loginWidgetNode lw) } }
            , _loggedInWidgetNode = (undefined,return)
            , _setLoggedInWidgetNode = \n fs ->
                    let m = view fs
                        lw = _loginWidget m
                    in return $ fs .= m { _loginWidget = lw { _loggedInWidgetNode = (n, snd $ _loggedInWidgetNode lw) } }
            }
    }
