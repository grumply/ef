module Ef.State
  ( State, statep, state
  , pattern PutP, pattern GetP, getp, get
  , pattern Put, pattern Get, putp, put
  , pattern ModifyP, pattern Modify, modifyp, modify
  ) where

import Ef

data State p s k where
  State
    :: { state_proxy :: Proxy p
       , currentState :: s
       , stateViewer :: k
       , stateSetter :: s -> k
       } -> State p s k



  Get_
    :: { get_state_proxy :: Proxy p
       , viewState :: s -> k
       } -> State p s k

  Put_
    :: { put_state_proxy :: Proxy p
       , newState :: s
       , result :: k
       } -> State p s k

  Modify_
    :: { modify_state_proxy :: Proxy p
       , stateModifier :: s -> s
       , result :: k
       } -> State p s k

  deriving Functor

pattern PutP p n = Put_ p n (Return ())
pattern GetP p f = Get_ p f
pattern ModifyP p f = Modify_ p f (Return ())

pattern Put n <- Put_ (Proxy :: Proxy ()) n (Return ()) where
  Put n = Put_ unit n (Return ())

pattern Get f <- Get_ (Proxy :: Proxy ()) f where
  Get f = Get_ unit f

pattern Modify f <- Modify_ (Proxy :: Proxy ()) f (Return ()) where
  Modify f = Modify_ unit f (Return ())

instance Delta (State p s) (State p s) where
  delta eval State {..} Get_ {..} = eval stateViewer (viewState currentState)
  delta eval State {..} Put_ {..} = eval (stateSetter newState) result
  delta eval State {..} Modify_ {..} = eval (stateSetter (stateModifier currentState)) result

statep :: forall p ts st c. (Applicative c, '[State p st] <. ts) => Proxy p -> st -> State p st (Action ts c)
statep p = fix $ \mk current -> State p current pure $ \new -> pure . Module (mk new)

state :: forall ts st c. (Applicative c, '[State () st] <. ts) => st -> State () st (Action ts c)
state = statep unit

getp :: forall p st ms c. (Monad c, '[State p st] <: ms) => Proxy p -> Code ms c st
getp = Send . flip GetP Return

get :: forall st ms c. (Monad c, '[State () st] <: ms) => Code ms c st
get = Send (GetP unit Return)

putp :: forall p st ms c. (Monad c, '[State p st] <: ms) => Proxy p -> st -> Code ms c ()
putp p = Send . PutP p

put :: forall st ms c. (Monad c, '[State () st] <: ms) => st -> Code ms c ()
put = Send . PutP unit

modifyp :: forall p st ms c. (Monad c, '[State p st] <: ms) => Proxy p -> (st -> st) -> Code ms c ()
modifyp p = Send . ModifyP p

modify :: forall st ms c. (Monad c, '[State () st] <: ms) => (st -> st) -> Code ms c ()
modify = Send . ModifyP unit

