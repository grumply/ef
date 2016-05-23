module Ef.Set (Set, set, become) where

import Ef

import Unsafe.Coerce

data Set k where
    Set :: (Object contexts environment -> k) -> Set k
    Become :: Object traits super -> k -> Set k

instance Ma Set Set where
    ma use (Set ok) (Become o k) = use (ok (unsafeCoerce o)) k

set :: (Monad super, '[Set] <. traits)
    => Trait Set traits super
set = Set (const . pure)

become :: (Monad super, '[Set] <: self, Ma (Traits traits) (Messages self))
       => Object traits super -> Narrative self super ()
become = self . flip Become ()
