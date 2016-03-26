{-# language RecordWildCards #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language ExistentialQuantification #-}
{-# language ScopedTypeVariables #-}
{-# language NoMonomorphismRestriction #-}
{-# language PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Menu where

import Ef
import Ef.Event

import Lotus
import Lily

import qualified GHCJS.DOM.Element as E
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

-- data Button btn k
--     = Button
--           { _buttonNode :: (Node,k)
--           , _setButtonNode :: Node -> k
--           , _buttonClicks :: forall self super. (Signal self super T.MouseEvent,k)
--           , _setButtonClicks :: forall self super. Signal self super T.MouseEvent -> k
--           }

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
          , _menuInitialized   :: (Bool,k)
          , _setMenuInitialized:: k
          , _loginWidget       :: LoginWidget k
          }
    | MenuNode (Node -> k)
    | SetMenuNode Node k
    | MenuInitialized (Bool -> k)
    | SetMenuInitialized k

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

menuInitialized = self (MenuInitialized id)

setMenuInitialized = self (SetMenuInitialized ())

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
    ma use Menu{..} (MenuInitialized bk) =
        use (snd _menuInitialized) (bk $ fst _menuInitialized)
    ma use Menu{..} (SetMenuInitialized k) =
        use _setMenuInitialized k

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
    , _menuInitialized = (False,return)
    , _setMenuInitialized = \fs ->
          let m = view fs
          in return $ fs .= m { _menuInitialized = (True,snd $ _menuInitialized m) }

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

noTextDecoration = [style "text-decoration" "none"]

font family weight color size =
    [ style "font-family" family
    , style "font-weight" weight
    , style "color" color
    , style "font-size" size
    ]

timesNewRoman weight color size =
    font "\"Times New Roman\", Serif" weight color size

helveticaNeue weight color size =
    font "\"Helvetica Neue\",Helvetica,Arial" weight color size

brandLetterStyling =
    [ style "padding" "2px 5px"
    , style "background-color" "darkcyan"
    ]

brandNamePadding =
    [ style "padding-left" "12px"
    ]

displayInline =
    [ style "display" "inline"
    ]

splitBrandStyles =
    [ style "top" "4px"
    , style "position" "relative"
    ]

navLinkStyles =
    noTextDecoration ++
    [ style "margin-right" "5px"
    , style "font-size" "14px"
    , style "color" "gray"
    ]

loginLinkStyles =
    noTextDecoration ++
    [ style "margin-top" "14px"
    ]

dividerStyles =
    [ style "border" "0"
    , style "height" "1px"
    , style "background-image"
            "linear-gradient(to right,\
            \  rgba(0,0,0,0),\
            \  rgba(0,0,0,0.75),\
            \  rgba(0,0,0,0)\
            \)"
    ]

initializeMenu = do
    with "lotus" $ do
        (menuRoot,_) <- create "nav" (Just "nav") $ do
            row
            (_,_) <- child "div" (Just "hat") $ do
                col XS Nothing
                child "a" (Just "brand-letter") $ do
                    setAttr "href" "#"
                    addStyles $
                        noTextDecoration ++
                        brandLetterStyling ++
                        timesNewRoman "bold" "white" "15px"
                    setText "O"
                child "a" (Just "brand-name") $ do
                    setAttr "href" "#"
                    addStyles $ brandNamePadding ++ noTextDecoration ++ displayInline
                    child "span" Nothing $ do
                        setText "Obvy"
                        addStyles $ helveticaNeue "600" "black" "13px"
                    child "span" Nothing $ do
                        setText "|"
                        addStyles $ splitBrandStyles ++ helveticaNeue "200" "rgba(0,0,0,0.5)" "28px"
                    child "span" Nothing $ do
                        setText "us"
                        addStyles $ helveticaNeue "600" "black" "13px"
                child "span" Nothing $ addStyle "padding" "0px 10px"
                ahref "Interesting" "/interesting" $ addStyles navLinkStyles
                ahref "Provacative" "/provacative" $ addStyles navLinkStyles
            ahref "Login" "/login" $ do
                end
                addStyles $
                    helveticaNeue "600" "gray" "13px" ++
                    loginLinkStyles
        super $ setMenuNode menuRoot
    setMenuInitialized

ahref txt lnk custom = do
    let hashPath = "#" ++ lnk
    child "a" Nothing $ do
        setAttr "href" hashPath
        setText txt
        custom
    return ()

divider = do
    (dv,_) <- create "div" Nothing $ do
        row
        child "div" Nothing (column 1)
        child "div" Nothing $ do
            column 10
            child "hr" Nothing $ addStyles dividerStyles
        child "div" Nothing (column 1)
    return dv
