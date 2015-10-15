{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Effect.Actor where

import Mop
import Mop.IO
import Effect.Exception
import Effect.Weave

import Control.Concurrent
import Control.Monad

import Data.Dynamic
import Data.IORef

import Unsafe.Coerce

import Data.Binary

data BoundedQueue a = Queue Int Int [a] [a]

emptyQueue n = Queue n 0 [] []

enqueue new q@(Queue mx cur l r)
  | mx == cur = Left (q,new)
  | otherwise = Right (Queue mx (cur+1) l (new:r))

dequeue (Queue _ 0 _ _) = Nothing
dequeue (Queue x n l (r:rs)) =
  Just (r,Queue x (n - 1) l rs)
dequeue (Queue x n [] r) =
  let l = reverse r
  in Just (head l,Queue x (n - 1) (tail l) [])

type Name = String

data Message = Local ActorRef Dynamic

data ActorRef
  = ActorRef { actorRef :: (Name,MVar Message) }

newtype Promise a = Promise { getPromise :: MVar a }

newPromiseIO :: IO (Promise a)
newPromiseIO = Promise <$> newEmptyMVar

newPromise :: (MIO m,Has Throw fs m) => PlanT fs m (Promise a)
newPromise = mio newPromiseIO

demand :: (MIO m,Has Throw fs m) => Promise a -> PlanT fs m a
demand = mio . demandIO

demandIO :: Promise a -> IO a
demandIO = readMVar . getPromise

fulfill :: (MIO m,Has Throw fs m) => Promise a -> a -> PlanT fs m Bool
fulfill = (mio .) . fulfillIO

fulfillIO :: Promise a -> a -> IO Bool
fulfillIO (Promise p) a = tryPutMVar p a

data Actor k
  = forall hs is m' a.
    (Pair (Instrs is) (Symbol hs),Has Actor hs m',Has Weave hs m',Has Throw hs m',MIO m')
    => Supervisor
         Name
         (forall x. m' x -> IO x)
         (forall b. PlanT hs m' b -> PlanT hs m' b)
         (InstructionsT is m')
         (Consumer hs Message m' a)
         k

  | forall m'' js ks a.
    (Pair (Instrs ks) (Symbol js),Has Weave js m'',Has Actor js m'',Has Throw js m'',MIO m'')
    => Actor
         Name
         (forall x. m'' x -> IO x)
         (forall b. PlanT js m'' b -> PlanT js m'' b)
         (InstructionsT ks m'')
         (Consumer js Message m'' a)
         k
  | Lookup String (Maybe ActorRef -> k)
  | forall a. Typeable a => Send a ActorRef ActorRef k
  | Self (ActorRef -> k)
  | GetChildren ([ActorRef] -> k)
  | GetParent (ActorRef -> k)

lookup :: Has Actor fs m => String -> PlanT fs m (Maybe ActorRef)
lookup str = symbol (Lookup str id)

send :: (Has Actor fs m,Typeable a) => a -> ActorRef -> PlanT fs m ()
send a to = symbol (Send a undefined to ())

self :: (Has Actor fs m) => PlanT fs m ActorRef
self = symbol (Self id)

getChildren :: (Has Actor fs m) => PlanT fs m [ActorRef]
getChildren = symbol (GetChildren id)

getParent :: (Has Actor fs m) => PlanT fs m ActorRef
getParent = symbol (GetParent id)

data Actors k = Actors k

type Creator
  = forall m' m'' hs js ks a.
    ( Pair (Instrs ks) (Symbol js)
    , Has Throw hs m'
    , Has Actor hs m'
    , Has Weave hs m'
    , MIO m'
    , Has Throw js m''
    , Has Actor js m''
    , Has Weave js m''
    , MIO m''
    ) => Name
      -> (forall x. m'' x -> IO x)
      -> ( forall b.
              PlanT js m'' b
           -> PlanT js m'' b
         )
      -> InstructionsT ks m''
      -> Consumer js Message m'' a
      -> PlanT hs m' (ActorRef,Promise (Either SomeException a))

type SupervisorCreator = Creator
type ActorCreator = Creator

produceMVar :: (Has Weave fs m,Has Throw fs m,MIO m) => MVar a -> Producer fs a m r
produceMVar mv = producer go
  where
    go yield = go'
      where
        go' = do
          a <- mio $ takeMVar mv
          yield a
          go'

send_ :: Typeable a => a -> ActorRef -> ActorRef -> IO ()
send_ a ar (ActorRef (_,mv)) = putMVar mv (Local ar (toDyn a))

-- | system is the initialization point for an actor system.
--
-- @
-- system $ \supervisor actor -> do
--   sObj <- createNewSupervisorObjEffectfully
--   someSupervisor <- supervisor "someSupervisor" id supervisionStrategy0 sObj $ do
--                       aObj <- createNewActorObjEffectfully
--                       someActor <- actor "someActor" id supervisionStrategy1 aObj
--                       ...
-- @
system :: forall fs m a. (Has Actor fs m, Has Throw fs m, Has Weave fs m, MIO m)
       => (SupervisorCreator -> ActorCreator -> Consumer fs Message m a)
       -> PlanT fs m a
system x = do
    (ars,rt) <- mio $ do mv <- newEmptyMVar
                         let root = ActorRef ("/",mv)
                         ars <- newIORef [root]
                         return (ars,root)
    let ActorRef (_,mv) = rt
    system_ rt ars $ do
      let cnsmr = x (\nm lft strat obj cnsmr -> symbol (Supervisor nm lft strat obj cnsmr undefined))
                    (\nm lft strat obj cnsmr -> symbol (Actor nm lft strat obj cnsmr undefined))
      linearize (produceMVar mv >-> cnsmr)
  where
    system_ :: ActorRef -> IORef [ActorRef] -> PlanT fs m a -> PlanT fs m a
    system_ root ars = go []
      where
        go chldrn = go'
          where
            go' =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Supervisor nm lft strat obj cnsmr _ -> do
                          (ar,p) <- mio $ do
                            mv <- newEmptyMVar
                            let ar = ActorRef (nm,mv)
                            p <- newPromiseIO
                            tid <- forkIO $ do
                                     (_,esa) <- lft $ delta obj (try $ supervisor_ ar root ars $ strat $ linearize $ produceMVar mv >-> cnsmr)
                                     case esa of
                                       Left (se :: SomeException) -> send_ se ar root
                                       Right a -> void $ fulfillIO p a
                            return (ar,p)
                          go' (bp (unsafeCoerce (ar,p)))
                        Actor nm lft strat obj cnsmr _ -> do
                          -- system-level actor; unsupervised
                          (ar,p) <- mio $ do
                            mv <- newEmptyMVar
                            let ar = ActorRef (nm,mv)
                            p <- newPromiseIO
                            tid <- forkIO $ do
                                     (_,esa) <- lft $ delta obj (try $ systemActor_ ar root ars $ strat $ linearize $ produceMVar mv >-> cnsmr)
                                     case esa of
                                       Left (se :: SomeException) -> send_ se ar root
                                       Right a -> void $ fulfillIO p a
                            return (ar,p)
                          go (bp (unsafeCoerce (ar,p)))
                        Lookup str _ -> do
                          refs <- mio (readIORef ars)
                          let as = filter (\(ActorRef (nm,mv)) -> nm == str) refs
                          case as of
                            [] -> go (bp (unsafeCoerce Nothing))
                            (a:as) -> go (bp (unsafeCoerce (Just a)))
                        Send a _ to _ -> do
                          mio (send_ a root to)
                          go (bp (unsafeCoerce to))
                        GetChildren _ -> do
                          chldrn <- mio (readIORef ars)
                          go (bp (unsafeCoerce chldrn))
                        GetParent _ -> go (bp (unsafeCoerce root))
                M m -> M (fmap go m)
                Pure r -> Pure r

    supervisor_ slf parent ars = go []
      where
        go chldrn = go'
          where
            go' p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Supervisor nm lft strat obj cnsmr _ -> undefined
                        Actor nm lft strat obj cnsmr _ -> undefined
                        Lookup str _ -> undefined
                        Send a _ to _ -> undefined
                        GetChildren _ -> undefined
                        GetParent _ -> undefined

    systemActor_ slf root ars = go []
      where
        go chldrn = go'
          where
            go' p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Lookup str _ -> undefined

    actor_ slf parent ars = go []
      where
        go chldrn = go'
          where
            go' p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        Lookup str _ -> undefined
