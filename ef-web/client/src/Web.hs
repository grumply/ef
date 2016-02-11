{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web (module Web, JSType.JSVal, JSType.ToJSVal(..), JSType.FromJSVal(..))where

import Ef
import Ef.IO

import qualified GHCJS.Marshal      as JSType
import qualified GHCJS.Types        as JSType
import qualified GHCJS.DOM.Types    as JSType
import qualified GHCJS.DOM          as DOM
import qualified GHCJS.DOM.Window   as Window
import qualified GHCJS.DOM.Screen   as Screen
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Element  as Element

data Window k = Window
    { window   :: (Window.Window,k)
    , screen   :: (Screen.Screen,k)
    , document :: (Document.Document,k)
    , body     :: (JSType.HTMLElement,k)
    }

initWindow :: (Subclass '[Window] methods)
           => IO (Method Window methods IO)
initWindow = do
    Just w <- DOM.currentWindow
    Just d <- DOM.currentDocument
    Just s <- Window.getScreen w
    Just b <- Document.getBody d
    let window   = (w,return)
        screen   = (s,return)
        document = (d,return)
        body     = (b,return)
    return Window {..}

newtype Width = Width Int deriving (Eq,Ord)

newtype Height = Height Int deriving (Eq,Ord)

newtype Distance = Distance Int deriving (Eq,Ord)

data Orientation
    = Portrait  | PortraitUpsideDown
    | Landscape | LandscapeUpsideDown
    deriving Eq

data Windowly k
    = GetScreen   (Screen.Screen   -> k)
    | GetWindow   (Window.Window   -> k)
    | GetDocument (Document.Document -> k)
    | GetBody     (JSType.HTMLElement -> k)

instance Ma Window Windowly where
    ma use Window {..} (GetScreen sk) =
        use (snd screen) (sk $ fst screen)

    ma use Window {..} (GetWindow wk) =
        use (snd window) (wk $ fst window)

    ma use Window {..} (GetDocument dk) =
        use (snd document) (dk $ fst document)

    ma use Window {..} (GetBody bk) =
        use (snd body) (bk $ fst body)

type Windowed self super result =
    ( '[Windowly] <: self
    , Monad super
    , Lift IO super
    ) => Narrative self super result

-- | Window scope accessor
getWindow :: Windowed self super Window.Window
getWindow = self (GetWindow id)

-- | Screen scope accessor
getScreen :: Windowed self super Screen.Screen
getScreen = self (GetScreen id)

-- | Document scope accessor
getDocument :: Windowed self super Document.Document
getDocument = self (GetDocument id)

-- | Body scope accessor
getBody :: Windowed self super JSType.HTMLElement
getBody = self (GetBody id)

-- | Window usage pattern method
withWindow :: (Window.Window -> IO a) -> Windowed self super a
withWindow f = io . f =<< getWindow

-- | Screen usage pattern method
withScreen :: (Screen.Screen -> IO a) -> Windowed self super a
withScreen f = io . f =<< getScreen

-- | Document usage pattern method
withDocument :: (Document.Document -> IO a) -> Windowed self super a
withDocument f = io . f =<< getDocument

-- | Body usage pattern method
withBody :: (JSType.HTMLElement -> IO a) -> Windowed self super a
withBody f = io . f =<< getBody

--------------------------------------------------------------------------------
-- Dimensionality queries.


-- | Width, in pixels, of the screen. Note that not all of the
-- width given by this method may be available to the window itself.
-- In those cases, `screenWidth` will differ from `screenAvailWidth`.
screenWidth :: Windowed self super Int
screenWidth =
    withScreen (fmap fromIntegral . Screen.getWidth)


-- | Horizontal space, in pixels, available to the window.
screenAvailWidth :: Windowed self super Int
screenAvailWidth =
    withScreen (fmap fromIntegral . Screen.getAvailWidth)


-- | Width, in pixels, of the outside of the browser window. This value
-- includes all window borders and stylings.
windowOuterWidth :: Windowed self super Int
windowOuterWidth =
    withWindow Window.getOuterWidth


-- | Width, in pixels, of the browsing viewport including, if visible,
-- the vertical scrollbar.
windowInnerWidth :: Windowed self super Int
windowInnerWidth =
    withWindow Window.getInnerWidth


-- | Horizontal distance, in CSS pixels, of the left border of the browser
-- from the left side of the screen.
screenX :: Windowed self super Int
screenX =
    withWindow Window.getScreenX


-- | Inner width, in pixels, of an element including padding but excluding
-- the vertical scrollbar, border, or margin. Returns 0 for elements with
-- no CSS or inline layout boxes. 
bodyClientWidth :: Windowed self super Int
bodyClientWidth =
    withBody (fmap round . Element.getClientWidth)


-- | Layout width of the body which includes the body's borders, horizontal
-- padding, vertical scrollbar if visible, and the CSS width.
bodyOffsetWidth :: Windowed self super Int
bodyOffsetWidth =
    withBody (fmap round . Element.getOffsetWidth)


-- | Height, in pixels, of the screen. Note that not all of the
-- height given by this method may be available to the window itself.
-- Taskbars and other special windows may reduce this value. In those
-- cases, `screenHeight` will differ from `screenAvailHeight`.
screenHeight :: Windowed self super Int
screenHeight =
    withScreen (fmap fromIntegral . Screen.getHeight)


-- | Vertical space, in pixels, available to the window.
screenAvailHeight :: Windowed self super Int
screenAvailHeight =
    withScreen (fmap fromIntegral . Screen.getAvailWidth)


-- | Height, in pixels, of the outside of the browser window. This value
-- includes all window borders and styling.
windowOuterHeight :: Windowed self super Int
windowOuterHeight =
    withWindow Window.getOuterHeight


-- | Height, in pixels, of the browsing viewport including, if visible,
-- the horizontal scrollbar.
windowInnerHeight :: Windowed self super Int
windowInnerHeight =
    withWindow Window.getInnerHeight


-- | Vertical distance, in CSS pixels, of the top border of the browser
-- from the top edge of the screen.
screenY :: Windowed self super Int
screenY =
    withWindow Window.getScreenY


-- | Inner height, in pixels, of an element including padding but excluding
-- the horizontal scrollbar, border, or margin. Returns 0 for elements with
-- no CSS or inline layout boxes.
bodyClientHeight :: Windowed self super Int
bodyClientHeight =
    withBody (fmap round . Element.getClientHeight)


-- | Layout height of the body which includes the body's borders, vertical
-- padding, horizontal scrollbar if visible, and the CSS height.
bodyOffsetHeight :: Windowed self super Int
bodyOffsetHeight =
    withBody (fmap round . Element.getOffsetHeight)

