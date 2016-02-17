module Queue (newVar,arrive,collect,Queue,newBarrier,Barrier,signalBarrier,waitBarrier,Var,newVar,modifyMVar,modifyVar_,readVar) where


import Control.Concurrent
import Control.Monad


type Var a = MVar a

newVar :: a -> IO (Var a)
newVar = newMVar

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar = modifyMVar

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ = modifyMVar_

readVar :: Var a -> IO a
readVar = readMVar

type Barrier a = MVar a

newBarrier :: IO (Barrier a)
newBarrier = newEmptyMVar

signalBarrier :: Barrier a -> a -> IO ()
signalBarrier = putMVar

waitBarrier :: Barrier a -> IO a
waitBarrier = readMVar

type Queue a = Var (Either [a] (Barrier [a]))

arrive :: Queue event -> event -> IO ()
arrive q x = modifyVar_ q $ \q ->
    case q of
        Left xs ->
            return $ Left $ x:xs
        Right b -> do
            signalBarrier b [x]
            return $ Left []

collect :: Queue event -> IO [event]
collect q = join $ modifyVar q $ \q ->
    case q of
        Left xs@(_:_) ->
            return (Left [], return $ reverse xs)
        _ -> do case q of
                    Right b -> signalBarrier b []
                    _ -> return ()
                b <- newBarrier
                return (Right b, waitBarrier b)
