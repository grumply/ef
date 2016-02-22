{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Templating.Simple where

import Ef
import HTML

import Data.List


classy :: Monad super
       => String -> String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
classy element cls f = child element (setClass cls >> f)

fonts :: [String] -> String
fonts fs = ('\"':intercalate "\", \"" fs) ++ "\""


url :: Monad super => Narrative self super String -> Narrative '[HTML self super] (Narrative self super) String
url f = do
    i <- super f
    return $ "url('" ++ i ++ "')"


simple :: Monad m => m ()
simple = return ()


_nav :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_nav = classy "nav"


_header :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_header = classy "header"


_div :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_div = classy "div"


_div_id :: Monad super => String -> String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_div_id cls i f = classy "div" cls (identity i >> f)


_button :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_button cls f = classy "button" cls (setAttr "type" "button" >> f)


_button_id :: Monad super => String -> String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_button_id cls i f = classy "button" cls (setAttr "type" "button" >> identity i >> f)


_span :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_span = classy "span"


_a :: Monad super => String -> String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_a cls href f = classy "a" cls (setAttr "href" href >> f)


_img :: Monad super => String -> String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_img cls src f = classy "img" cls (setAttr "src" src >> f)


_i :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_i cls f = classy "i" cls f


_ul :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_ul = classy "ul"


_li :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_li = classy "li"


identity :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) ()
identity = setAttr "id"


text :: Monad super => String -> Narrative '[HTML self super] (Narrative self super) ()
text = setText


_h1 :: Monad super => String -> String -> Narrative '[HTML self super] (Narrative self super) a -> Narrative '[HTML self super] (Narrative self super) (Element self super,a)
_h1 cls txt f = classy "h1" cls $ do
    text txt
    f
