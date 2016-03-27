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

import Dahlia

import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Types as T

import Control.Monad
import Data.List

import Unsafe.Coerce

import Prelude

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

mkMenu :: forall self super methods.
        (Ma (Methods methods) (Messages self), Monad super, '[Menu] <: self, Subclass '[Menu] methods)
     => Menu (Implementation methods super)
mkMenu = Menu
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

--------------------------------------------------------------------------------

noTextDecoration = "text-decoration" =: "none"

timesNewRoman weight color size =
    font "\"Times New Roman\", Serif" weight color size

helveticaNeue weight color size =
    font "\"Helvetica Neue\",Helvetica,Arial" weight color size

brandLetterStyling = do
    "background-color" =: "darkcyan"
    "padding" =: "2px 5px"
    "margin-right" =: "10px"

navLinkStyles = do
    noTextDecoration
    "font-size" =: "14px"
    "color" =: "gray"


dividerStyles = do
    "border" =: "0"
    "height" =: "1px"
    "background-image" =:
            "linear-gradient(to right,\
            \  rgba(0,0,0,0),\
            \  rgba(0,0,0,0.75),\
            \  rgba(0,0,0,0)\
            \)"

--------------------------------------------------------------------------------

logo = ahref_ "O" "" $ style $ do
    noTextDecoration
    brandLetterStyling
    timesNewRoman "bold" "white" "15px"

--------------------------------------------------------------------------------

brand = a_ $ do
    setAttr "href" "#"
    style $ do
        noTextDecoration
        "margin-right" =: "16px"
    linkText "Obvy"
    separator
    linkText "us"
    where
        linkText txt = span_ $ do
            text txt
            style $ helveticaNeue "600" "black" "13px"
        separator = span_ $ do
            text "|"
            style $ do
                helveticaNeue "200" "rgba(0,0,0,0.5)" "28px"
                "position" =: "relative"
                "top" =: "4px"

--------------------------------------------------------------------------------

links = do
    ahref__ "Interesting" "/interesting" $ style navLinkStyles
    span_ $ style $ "margin" =: "0 6px"
    ahref__ "Provacative" "/provacative" $ style navLinkStyles

--------------------------------------------------------------------------------

loginLink = a_ $ do
    setAttr "href" "#/login"
    style $ "margin-right" =: "-4px"
    span_ $ do
        setAttr "class" "glyphicons glyphicons-log-in"
        style loginGlyphStyles
    where

        loginGlyphStyles = do
            "top" =: "6px"
            "position" =: "relative"
            "text-decoration" =: "none"
            "font-size" =: "20px"
            "color" =: "darkcyan"

--------------------------------------------------------------------------------

menu = nav__ "menu" $ do
    style row
    hat
    login
    where

        hat = column_ 10 $ do
            style start
            logo
            brand
            links

        login = column_ 2 $ do
            style end
            loginLink

--------------------------------------------------------------------------------

initializeMenu = do
    with "lotus" $ do
        (mn,_) <- menu
        super $ setMenuNode mn
    setMenuInitialized

--------------------------------------------------------------------------------

divider =
    divRow_ $ do
        column_ 1 (return ())
        column_ 10 $ hr_ (style dividerStyles)
        column_ 1 (return ())

footer = do
    let footerLinkStyles = do
            "text-decoration" =: "none"
            "color" =: "gray"
            "font-size" =: "12px"
        footerLink txt lnk = void $ ahref_ txt lnk $ style footerLinkStyles
        footerSeparatorStyles = do
            "color" =: "gray"
            "font-size" =: "12px"
            "margin" =: "0px 10px"
        footerSeparator = void $ a_ $ do
            -- convert this to some non-content element.
            style footerSeparatorStyles
            text "|"
    divRow_ $ do
        column_ 12 $ do
            style center
            unorderedList_ (return ()) $
                intersperse footerSeparator
                    [ footerLink "About" "/about"
                    , footerLink "Contact" "/contact"
                    , footerLink "Privacy" "/privacy"
                    ]
        column_ 12 $ do
            style center
            span_ $ do
                style $ do
                    "color" =: "gray"
                    "font-size" =: "12px"
                text "© 2016 S. M. Hickman"


-- Anonymous element creation | Level 1

-- Note the difference between anon_ and nonAnon__. anon_ does not return the
-- created node, only the return value of the content passed to it. nonAnon__
-- on the other hand does return the created node as well as the return value
-- of the content passed to it.
anon_ :: (Monad super)
      => String
      -> Narrative '[Camellia] (Narrative self super) a
      -> Narrative '[Camellia] (Narrative self super) a
anon_ x = fmap snd . child x Nothing

div_ = anon_ "div"

span_ = anon_ "span"

nav_ = anon_ "nav"

ul_ = anon_ "ul"

ol_ = anon_ "ol"

li_ = anon_ "li"

p_ = anon_ "p"

a_ = anon_ "a"

hr_ = anon_ "hr"

-- Anonymous element creation | Level 2

unorderedList_ listStyles listItems =
    ul_ $ do
        listStyles
        mapM_ li_ listItems

divRow_ content =
    div_ $ do
        style row
        content

column_ n content =
    div_ $ do
        column n
        content

ahref_ txt lnk custom = do
    let hashPath = "#" ++ lnk
    a_ $ do
        setAttr "href" hashPath
        text txt
        custom

-- Non-Anonymous element creation | Level 1

-- Note the difference between nonAnon__ and anon_. anon_ does not return the
-- created node, only the return value of the content passed to it. nonAnon__
-- on the other hand does return the created node as well as the return value
-- of the content passed to it.
nonAnon__ :: (Monad super)
          => String
          -> String
          -> Narrative '[Camellia] (Narrative self super) a
          -> Narrative '[Camellia] (Narrative self super) (Node,a)
nonAnon__ x ident = child x (Just ident)

div__ = nonAnon__ "div"

span__ = nonAnon__ "span"

nav__ = nonAnon__ "nav"

ul__ = nonAnon__ "ul"

ol__ = nonAnon__ "ol"

li__ = nonAnon__ "li"

p__ = nonAnon__ "p"

a__ = nonAnon__ "a"

hr__ = nonAnon__ "hr"

-- Non-Anonymous element creation | Level 2

ahref__ txt lnk custom = do
    let hashPath = "#" ++ lnk
    a__ (txt ++ "Link") $ do
        setAttr "href" hashPath
        text txt
        custom
