module Ef.State
  ( State, statep, state
  , pattern PutP, pattern GetP, getp, get
  , pattern Put, pattern Get, putp, put
  , pattern ModifyP, pattern Modify, modifyp, modify
  ) where

import Ef

data State p s k where
  State
    :: { state_proxy :: {-# UNPACK #-} !(Proxy p)
       , currentState :: !s
       , stateViewer :: !k
       , stateSetter :: !(s -> k)
       } -> State p s k



  Get_
    :: { get_state_proxy :: {-# UNPACK #-} !(Proxy p)
       , viewState :: !(s -> k)
       } -> State p s k

  Put_
    :: { put_state_proxy :: {-# UNPACK #-} !(Proxy p)
       , newState :: !s
       , result :: !k
       } -> State p s k

  Modify_
    :: { modify_state_proxy :: {-# UNPACK #-} !(Proxy p)
       , stateModifier :: !(s -> (s,a))
       , viewModified :: !((s,a) -> k)
       } -> State p s k

instance Functor (State p s) where
  fmap f (State p s k sk) = State p s (f k) (fmap f sk)
  fmap f (Get_ p sk) = Get_ p (fmap f sk)
  fmap f (Put_ p s k) = Put_ p s (f k)
  fmap f (Modify_ p ssa sak) = Modify_ p ssa (fmap f sak)

pattern PutP p n = Put_ p n (Return ())
pattern GetP p f = Get_ p f
pattern ModifyP p f r = Modify_ p f r

pattern Put n <- Put_ (Proxy :: Proxy ()) n (Return ()) where
  Put n = Put_ unit n (Return ())

pattern Get f <- Get_ (Proxy :: Proxy ()) f where
  Get f = Get_ unit f

pattern Modify f r <- Modify_ (Proxy :: Proxy ()) f r where
  Modify f r = Modify_ unit f r

instance Delta (State p s) (State p s) where
  delta eval State {..} Get_ {..} = eval stateViewer (viewState currentState)
  delta eval State {..} Put_ {..} = eval (stateSetter newState) result
  delta eval State {..} Modify_ {..} =
    let (new,a) = stateModifier currentState
    in eval (stateSetter new) (viewModified (new,a))

{-# INLINE statep #-}
statep :: forall p ts st c. (Monad c, '[State p st] <. ts) => Proxy p -> st -> State p st (Action ts c)
statep p initial =
  State p initial pure $ \new o -> return $ Module (statep p new) o

{-# INLINE state #-}
state :: forall ts st c. (Monad c, '[State () st] <. ts) => st -> State () st (Action ts c)
state = statep unit

{-# INLINE getp #-}
getp :: forall p st ms c. (Monad c, '[State p st] <: ms) => Proxy p -> Code ms c st
getp = Send . flip GetP Return

{-# INLINE get #-}
get :: forall st ms c. (Monad c, '[State () st] <: ms) => Code ms c st
get = Send (GetP unit Return)

{-# INLINE putp #-}
putp :: forall p st ms c. (Monad c, '[State p st] <: ms) => Proxy p -> st -> Code ms c ()
putp p = Send . PutP p

{-# INLINE put #-}
put :: forall st ms c. (Monad c, '[State () st] <: ms) => st -> Code ms c ()
put = Send . PutP unit

{-# INLIEN modifyp #-}
modifyp :: forall p st ms c a. (Monad c, '[State p st] <: ms) => Proxy p -> (st -> (st,a)) -> Code ms c (st,a)
modifyp p f = Send $ ModifyP p f Return

{-# INLINE modify #-}
modify :: forall st ms c a. (Monad c, '[State () st] <: ms) => (st -> (st,a)) -> Code ms c (st,a)
modify f = Send $ ModifyP unit f Return

