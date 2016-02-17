{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Event where

import Data.Typeable
import qualified GHCJS.DOM.Event as Event

data SomeEvent = forall e. (Event.IsEvent e,Typeable e) => SomeEvent e

class (Event.IsEvent e,Typeable e) => Event e where
    toEvent :: e -> SomeEvent
    toEvent = SomeEvent

    fromEvent :: SomeEvent -> Maybe e
    fromEvent (SomeEvent e) = cast e

instance (Event.IsEvent e,Typeable e) => Event e
