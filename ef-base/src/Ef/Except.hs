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

throwChecked :: e -> Ef (Throws e ': ms) c a
throwChecked = Send . Throw

catchChecked :: forall ms c a e. (Monad c, Functor (Messages ms))
             => Ef (Throws e ': ms) c a -> (e -> Ef ms c a) -> Ef ms c a
catchChecked act h = handleThrows act
  where
    handleThrows :: Ef (Throws e ': ms) c a -> Ef ms c a
    handleThrows = transform id go
      where
        go :: Messages (Throws e ': ms) (Ef (Throws e ': ms) c a) -> Ef ms c a
        go m =
          case prj m of
            Just (Throw e) -> h e
            _ ->
              case m of
                Other ms -> Do (fmap (transform id go) ms)

handleChecked :: forall ms c a e. (Monad c, Functor (Messages ms))
              => (e -> Ef ms c a) -> Ef (Throws e ': ms) c a -> Ef ms c a
handleChecked h act = catchChecked act h

tryChecked
    :: (Monad c, Functor (Messages ms))
    => (Ef (Throws e ': ms) c r)
    -> Ef ms c (Either e r)
tryChecked a = catchChecked (Right <$> a) (return . Left)

mapChecked
    :: (Monad c, Exception e, Exception e', Functor (Messages ms))
    => (e -> e')
    -> (Ef (Throws e ': Throws e' ': ms) c a)
    -> (Ef (Throws e' ': ms) c a)
mapChecked f p = catchChecked p (throwChecked . f)
