module Ef.Set (Set, set, become) where

import Ef

import Unsafe.Coerce

data Set k where
    Set :: (Object contexts environment -> k) -> Set k
    Become :: Object traits super -> k -> Set k

set :: Use Set contexts environment
set = Set (const . pure)

become :: Ma (Traits traits) (Messages self)
       => Object traits super
       -> Invoke Set self super ()

become = self . flip Become ()

instance Ma Set Set where
    ma use (Set ok) (Become o k) =
        let obj = unsafeCoerce o
        in use (ok obj) k
