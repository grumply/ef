module Ef.Except
     ( Throws
     , throwChecked
     , catchChecked
     , handleChecked
     , tryChecked
     , mapChecked
     , Exception(..)
     ) where

import Ef
import Control.Exception (Exception(..))

data Throws e k
  = Throw e
  deriving Functor

instance Delta (Throws e) (Throws e)

throwChecked :: e -> Code (Throws e ': ms) c a
throwChecked = Send . Throw

catchChecked :: forall ms c a e. (Monad c, Functor (Messages ms))
             => Code (Throws e ': ms) c a -> (e -> Code ms c a) -> Code ms c a
catchChecked act h = handleThrows act
  where
    handleThrows :: Code (Throws e ': ms) c a -> Code ms c a
    handleThrows = transform id go
      where
        go :: Messages (Throws e ': ms) (Code (Throws e ': ms) c a) -> Code ms c a
        go m =
          case prj m of
            Just (Throw e) -> h e
            _ ->
              case m of
                Other ms -> Do (fmap (transform id go) ms)

handleChecked :: forall ms c a e. (Monad c, Functor (Messages ms))
              => (e -> Code ms c a) -> Code (Throws e ': ms) c a -> Code ms c a
handleChecked h act = catchChecked act h

tryChecked
    :: (Monad c, Functor (Messages ms))
    => (Code (Throws e ': ms) c r)
    -> Code ms c (Either e r)
tryChecked a = catchChecked (Right <$> a) (return . Left)

mapChecked
    :: (Monad c, Exception e, Exception e', Functor (Messages ms))
    => (e -> e')
    -> (Code (Throws e ': Throws e' ': ms) c a)
    -> (Code (Throws e' ': ms) c a)
mapChecked f p = catchChecked p (throwChecked . f)
