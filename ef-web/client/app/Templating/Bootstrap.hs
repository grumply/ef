{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Templating.Bootstrap where

import Ef
import HTML
import Templating.Simple

import Control.Monad


container :: Monad super
          => Narrative '[HTML self super] (Narrative self super) a
          -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
container = _div "container"


topFixedInverseNavbar :: Monad super
                      => Narrative '[HTML self super] (Narrative self super) a
                      -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
topFixedInverseNavbar = _nav "navbar navbar-inverse navbar-fixed-top navbar-expanded"


navHeader :: Monad super
          => Narrative '[HTML self super] (Narrative self super) a
          -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
navHeader = _div "navbar-header"


collapsableNavbar :: Monad super
                  => Narrative '[HTML self super] (Narrative self super) a
                  -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
collapsableNavbar = _div_id "collapse navbar-collapse" "collapse-1"


navbarRight :: Monad super
            => Narrative '[HTML self super] (Narrative self super) a
            -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
navbarRight = _ul "nav navbar-nav navbar-right"


dataToggle :: Monad super
           => String
           -> Narrative '[HTML self super] (Narrative self super) ()
dataToggle = setAttr "data-toggle"


dataTarget :: Monad super
           => String
           -> Narrative '[HTML self super] (Narrative self super) ()
dataTarget = setAttr "data-target"


hamburger :: Monad super
          => Narrative '[HTML self super] (Narrative self super) (Element self super)
hamburger = do
    (e,_) <- _button "navbar-toggle" $ do
        dataToggle "collapse"
        dataTarget "#collapse-1"
        _span "sr-only" $ text "Toggle navigation"
        replicateM_ 3 $ _span "icon-bar" simple
    return e

hiddenScrollTop :: Monad super
                => Narrative '[HTML self super] (Narrative self super) (Element self super,Element self super)
hiddenScrollTop =
    _li "hidden active" $ do
        (lnk,_) <- _a "page-scroll" "#page-top" simple
        return lnk

pageLink :: Monad super
         => String
         -> Narrative '[HTML self super] (Narrative self super) a
         -> Narrative '[HTML self super] (Narrative self super) (Element self super,(Element self super,a))
pageLink ln f =
    _li "" $ _a "page-scroll" ('#':ln) $ do
        "transition" =: "all 0.5s"
        setText ln
        f

brandedPageTop :: Monad super
               => Narrative '[HTML self super] (Narrative self super) a
               -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
brandedPageTop f = _a "navbar-brand page-scroll" "#page-top" f
