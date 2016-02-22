{-# LANGUAGE FlexibleContexts #-}
module Web.Methods where

import Ef
import Ef.IO

import Signaled
import Queue

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Screen as Screen
import qualified GHCJS.DOM.RequestAnimationFrameCallback as RAF



import Control.Concurrent
import Data.Either
import Data.Function

data Web k = Web
    { window :: (Window.Window,k)
    , screen :: (Screen.Screen,k)
    , document :: (Document.Document,k)
    , drawer :: (Queue (Either (Double -> IO ()) (Double -> IO ())),k)
    , signaled :: (Signaled,k)
    }

createWeb :: Monad super => IO (Web (Implementation methods super))
createWeb = do 
    Just win <- DOM.currentWindow
    Just doc <- Window.getDocument win
    Just scr <- Window.getScreen win
    gb <- newVar (Left [])
    rafs <- newVar $ Left []
    
    forkIO $ fix $ \startRound -> do
        lrs <- collect rafs
        let (qs0,ws0) = partitionEithers lrs
        threadDelay 8000 -- 8 ms; this might need to be tuned down
        arrive rafs (Left $ \_ -> return ())
        lrs' <- collect rafs
        let (qs1,ws1) = partitionEithers lrs'
            (qs,ws) = (qs0 ++ qs1,ws0 ++ ws1)
        rafCallback <- RAF.newRequestAnimationFrameCallbackAsync $ \time -> do
            let writes = sequence_ (map ($ time) ws)
                queries = sequence_ (map ($ time) qs)
            writes
            queries
        Window.requestAnimationFrame win (Just rafCallback)
        startRound

    return $ Web (win,return) (scr,return) (doc,return) (rafs,return) (Signaled gb,return)
