module Data.Could
  ( Could(..), did, didn't, could've
  ) where

data Could x = Did x | Didn't

did :: Could x -> Bool
did (Did _) = True
did _ = False

didn't :: Could x -> Bool
didn't Didn't = True
didn't _ = False

could've :: y -> (x -> y) -> Could x -> y
could've _didn't _ Didn't = _didn't
could've _ _did (Did x) = _did x
