{-# LANGUAGE DeriveFunctor #-}
module Ef.Writer where

import Ef

import Data.Monoid

type WriterT w c a = Narrative (Writer w) c a
newtype Writer w k = Writer { runWriter :: w -> (w,k) }
  deriving Functor

{-# INLINE runWriterT #-}
runWriterT :: (Monad c, Monoid w) => Narrative (Writer w) c a -> c (w,a)
runWriterT n = foldn
  (\a w -> return (w,a))
  (\cf a -> cf >>= ($ a))
  (\wk w -> let (w',k) = runWriter wk w in k w')
  n
  mempty

{-# INLINE tell #-}
tell :: Monoid w => w -> Narrative (Writer w) c ()
tell w' = send (Writer (\w -> (w <> w',())))

