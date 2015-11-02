module Effect.Reactive where

import Mop.Core
import Effect.Interleave hiding (FreshScope)
import Unsafe.Coerce

newtype Event a = Event Int
newtype Behavior a = Behavior (Int,Int)

data React k
  = FreshScope (Int -> k)

  | forall a.               New Int (Event a -> k)
  | forall a.           Trigger Int (Event a) a k
  | forall fs m a.       Remove Int (Event a) k

  | forall fs m a b.   Register Int (Event a) (a -> Plan fs m ()) (Behavior a -> k)
  | forall fs m a.   Unregister Int (Behavior a) k

data Reactive k = Reactive Int k
reactive :: Uses Reactive fs m => Attribute Reactive fs m
reactive = Reactive 0 $ \fs ->
  let Reactive i k = (fs&)
      i' = succ i
  in i' `seq` pure (fs .= Reactive i' k)

instance Pair Reactive React where
  pair p (Reactive i k) (FreshScope ik) = p k (ik i)

data ReactScope fs m = React
  { newE        :: forall a. Plan fs m (Event a)
  , triggerE    :: forall a. Event a -> a -> Plan fs m ()
  , removeB     :: forall a. Event a -> Plan fs m ()
  , registerB   :: forall a. Event a -> (a -> Plan fs m ()) -> Plan fs m (Behavior a)
  , unregisterB :: forall a. Behavior a -> Plan fs m ()
  }

data ReactiveSubsystem fs m = Subsystem
  { eventScope :: Int
  , behaviorScope :: Int
  , reactions :: forall a. [(Event a,[(Behavior a,a -> Plan fs m ())])]
  }

react :: forall fs m a. (Has React fs m,Has Interleave fs m)
         => (    ReactScope fs m
              -> Plan fs m a
            )
         -> Plan fs m a
react f = do
  scope <- self (FreshScope id)
  transform scope $ f React
    { newE = self (New scope id)
    , triggerE = \ev a -> self (Trigger scope ev a ())
    , removeB = \ev -> self (Remove scope ev ())
    , registerB = \ev f -> self (Register scope ev f id)
    , unregisterB = \b -> self (Unregister scope b ())
    }
  where
    transform scope = go (Subsystem 0 0 [])
      where
        go sys = go'
          where
            go' p =
              case p of
                Step sym bp ->
                  case prj sym of
                    Just x ->
                      case x of
                        New i _ ->
                          if i == scope
                          then let eS = eventScope sys
                                   eS' = eS + 1
                               in eS' `seq` go (sys { eventScope = eS' })
                                               (bp (unsafeCoerce (Event eS)))
                          else Step sym (\b -> go' (bp b))
                        Trigger i ev a _ ->
                          if i == scope
                          then go' (triggerEvent ev a sys >> bp (unsafeCoerce ()))
                          else Step sym (\b -> go' (bp b))
                        Remove i ev _ ->
                          if i == scope
                          then go (removeEvent ev sys) (bp (unsafeCoerce ()))
                          else Step sym (\b -> go' (bp b))
                        Register i ev@(Event e) f _ ->
                          if i == scope
                          then let bS = behaviorScope sys
                                   bS' = bS + 1
                                   b = Behavior (e,bS)
                                   sys' = sys { behaviorScope = bS' }
                               in bS' `seq` go (registerBehavior b f sys')
                                               (bp (unsafeCoerce b))
                          else Step sym (\b -> go' (bp b))
                        Unregister i b _ ->
                          if i == scope
                          then go (unregisterBehavior b sys) (bp (unsafeCoerce ()))
                          else Step sym (\b -> go' (bp b))
                        _ -> Step sym (\b -> go' (bp b))
                    Nothing -> Step sym (\b -> go' (bp b))
                M m -> M (fmap go' m)
                Pure r -> Pure r

    triggerEvent (Event e) a sys = go (reactions sys)
      where
        go [] = return ()
        go (eb@(Event i,bs):ebs)
          | i == e = go' bs
          | otherwise = go ebs
          where
            go' [] = return ()
            go' ((_,f):bs) = (f (unsafeCoerce a)) >> go' bs

    removeEvent (Event e) sys = sys { reactions = go (reactions sys) }
      where
        go [] = []
        go (eb@(Event i,_):ebs)
          | i == e = ebs
          | otherwise = eb:go ebs

    registerBehavior b@(Behavior (_,bh)) f sys = sys { reactions = go (reactions sys) }
      where
        go [] = [(unsafeCoerce (Event bh),[(unsafeCoerce b,unsafeCoerce f)])]
        go (eb@(Event i,bs):ebs)
          -- performance leak to guarantee FIFO
          | i == bh = (Event i,bs ++ [(unsafeCoerce b,unsafeCoerce f)]):ebs
          | otherwise = eb:go ebs

    unregisterBehavior (Behavior (e,b)) sys = sys { reactions = go (reactions sys) }
      where
        go [] = []
        go (eb@(Event i,_):ebs)
          | i == e = go' eb ebs
          | otherwise = eb:go ebs

        go' eb@(_,[(Behavior (_,b'),_)]) ebs
          | b == b' = ebs
          | otherwise = eb:ebs
        go' eb@(ev,bs) ebs = (ev,go'' bs):ebs

        go'' [] = []
        go'' (bh@(Behavior (_,b'),_):bs)
          | b == b' = bs
          | otherwise = bh:go'' bs
