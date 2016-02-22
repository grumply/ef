{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Web.Messages where

import Ef
import Data.Promise

import Signaled
import Queue

import qualified GHCJS.Nullable as Null
import qualified GHCJS.Types as Ty
import qualified GHCJS.Marshal as Ty
import qualified GHCJS.DOM.Types as Ty
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Screen as Screen
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Element as Element

-- Current approach here is left open for further optimizations in terms of
-- monitoring changes. At some point, we could extend the drawer to allow
-- immediate queries if it thinks reflowing won't be necessary.

data Web k
    = GetScreen (Screen.Screen -> k)
    | GetWindow (Window.Window -> k)
    | GetDocument (Document.Document -> k)
    | GetDrawer (Queue (Either (Double -> IO ()) (Double -> IO ())) -> k)
    | GetSignaled (Signaled -> k)

getScreen :: ('[Web] <: self, Monad super)
          => Narrative self super Screen.Screen
getScreen = self (GetScreen id)

getWindow :: ('[Web] <: self, Monad super)
          => Narrative self super Window.Window
getWindow = self (GetWindow id)

getDocument :: ('[Web] <: self, Monad super)
            => Narrative self super Document.Document
getDocument = self (GetDocument id)

getDrawer :: ('[Web] <: self, Monad super)
          => Narrative self super (Queue (Either (Double -> IO ()) (Double -> IO ())))
getDrawer = self (GetDrawer id)

getSignaled :: ('[Web] <: self, Monad super)
            => Narrative self super Signaled
getSignaled = self (GetSignaled id)

query f = do
    drawer <- getDrawer
    lift $ do
        promise <- newPromiseIO
        arrive drawer $ Left $ \d -> do
            x <- f d
            fulfillIO promise x
            return ()
        return promise

write f = do
    drawer <- getDrawer
    lift $ do
        promise <- newPromiseIO
        arrive drawer $ Right $ \d -> do
            x <- f d
            fulfillIO promise x
            return ()
        return promise

foreign import javascript unsafe
    "$1.className += $2;"
    js_addClass :: Ty.JSVal -> Ty.JSString -> IO ()

rawAddClass :: ( Ty.IsElement elem
               , Ty.ToJSVal elem
               , Ty.ToJSString cls
               )
         => elem -> cls -> IO ()
rawAddClass el cls = do
    elem <- Ty.toJSVal el
    js_addClass elem (Ty.toJSString cls)

-- IE10+
foreign import javascript unsafe
    "$1.classList.remove($2);"
    js_removeClass :: Ty.JSVal -> Ty.JSString -> IO ()

rawRemoveClass :: ( Ty.IsElement elem
                  , Ty.ToJSVal elem
                  , Ty.ToJSString cls
                  )
               => elem -> cls -> IO ()
rawRemoveClass el cls = do
    elem <- Ty.toJSVal el
    js_removeClass elem (Ty.toJSString cls)

getAttr :: ( Lift IO super
           , Ty.ToJSString name
           , Ty.FromJSString result
           , Ty.IsElement element
           , Monad super
           , '[Web] <: self
           )
        => element -> name -> Narrative self super (Promise (Maybe result))
getAttr element name = query $ \_ -> Element.getAttribute element name

setAttr :: ( Lift IO super
           , Ty.ToJSString name
           , Ty.ToJSString value
           , Ty.IsElement element
           , Monad super
           , '[Web] <: self
           )
        => element -> name -> value -> Narrative self super (Promise ())
setAttr element name value = write $ \_ -> Element.setAttribute element name value

addClass :: ( Lift IO super
            , Ty.ToJSString cls
            , Ty.IsElement element
            , Monad super
            , '[Web] <: self
            )
         => element -> cls -> Narrative self super (Promise ())
addClass element cls = write $ \_ -> rawAddClass element cls

rawSetClass :: ( Ty.ToJSString cls
               , Ty.IsElement element
               )
            => element -> cls -> IO ()
rawSetClass = Element.setClassName

setClass :: ( Lift IO super
            , Ty.ToJSString cls
            , Ty.IsElement element
            , Monad super
            , '[Web] <: self
            )
         => element -> cls -> Narrative self super (Promise ())
setClass element cls = write $ \_ -> Element.setClassName element cls

removeClass :: ( Lift IO super
               , Ty.ToJSString cls
               , Ty.IsElement element
               , Monad super
               , '[Web] <: self
               )
            => element -> cls -> Narrative self super (Promise ())
removeClass element cls = write $ \_ -> rawRemoveClass element cls



foreign import javascript unsafe
    "$1.style[$2]=$3;"
    js_setStyle :: Ty.JSVal -> Ty.JSString -> Ty.JSString -> IO ()

rawSetStyle :: ( Ty.ToJSString key
               , Ty.ToJSString val
               , Ty.IsElement element
               )
         => element -> key -> val -> IO ()
rawSetStyle element k v = do
    e <- Ty.toJSVal element
    js_setStyle e (Ty.toJSString k) (Ty.toJSString v)
    
setStyle :: ( Lift IO super
            , Ty.ToJSString key
            , Ty.ToJSString val
            , Ty.IsElement element
            , Monad super
            , '[Web] <: self
            )
         => element -> key -> val -> Narrative self super (Promise ())
setStyle element k v = write $ \_ -> rawSetStyle element k v

foreign import javascript unsafe
    "$1.style[$2]=null;"
    js_removeStyle :: Ty.JSVal -> Ty.JSString -> IO ()

rawRemoveStyle :: ( Ty.ToJSString key
                  , Ty.IsElement element
                 )
               => element -> key -> IO ()
rawRemoveStyle element k = do
    e <- Ty.toJSVal element
    js_removeStyle e (Ty.toJSString k)

removeStyle :: ( Lift IO super
               , Ty.ToJSString key
               , Ty.IsElement element
               , Monad super
               , '[Web] <: self
               )
            => element -> key -> Narrative self super (Promise ())
removeStyle element k = write $ \_ -> rawRemoveStyle element k


foreign import javascript unsafe
    "$1.textContent=$2;"
    js_setText :: Ty.JSVal -> Ty.JSString -> IO ()

rawSetText :: ( Ty.ToJSString text
              , Ty.IsElement element
              )
           => element -> text -> IO ()
rawSetText element text = do
    e <- Ty.toJSVal element
    js_setText e (Ty.toJSString text)

setText :: ( Ty.ToJSString text
           , Ty.IsElement element
           , Monad super
           , Lift IO super
           , '[Web] <: self
           )
        => element -> text -> Narrative self super (Promise ())
setText element text = write $ \_ -> rawSetText element text
