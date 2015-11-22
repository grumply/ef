{-# LANGUAGE RankNTypes #-}
module Ef.Core.Codensity where



import Ef.Core.Pattern


import Control.Applicative
import Control.Monad



--------------------------------------------------------------------------------
-- | Codensity improves asymptotics of repeated left-associated binds by
--   conversion to right-associated binds.
--
-- This example demonstrates a performance improvement over the standard
-- replicateM.
--
-- >>> import Data.Time
-- >>> import System.Timeout
-- >>> :{
--   let
--     time f =
--         do
--           s <- getCurrentTime
--           r <- f
--           e <- r `seq` getCurrentTime
--           return (diffUTCTime e s,r)
--
--     replicateP n =
--         fromCodensity . Control.Monad.replicateM n . toCodensity
--
--   in
--     do
--       let
--         unit =
--             return ()
--
--         test f =
--             run (f 10 unit)
--
--       mResult <- time (run replicateM)
--       pResult <- time (run replicateP)
--       let
--         (conventionalTime,mRes) =
--             mResult
--
--         (codensityTime,pRes) =
--             pResult
--
--       return (conventionalTime - codensityTime > 0)
--
-- :}
--True

newtype Codensity fs m a =
    Codensity
        { runCodensity
              :: forall b.
                 (a -> Pattern fs m b)
              -> Pattern fs m b
        }



instance Functor (Codensity fs k)
  where

    fmap f (Codensity m) =
        Codensity $ m . (. f)



instance Applicative (Codensity fs f)
  where

    pure x = Codensity ($ x)



    (<*>) = ap



instance Monad (Codensity fs f)
  where

    return x =
        Codensity ($ x)



    m >>= k =
        Codensity $ \c ->

            runCodensity m $ \a ->
                runCodensity (k a) c



instance ( Alternative v
         , MonadPlus v
         )
    => Alternative (Codensity fs v)
  where

    empty = Codensity (\_ -> empty)



    Codensity m <|> Codensity n =
        Codensity $ \k ->
            m k <|> n k



instance MonadPlus v
    => MonadPlus (Codensity fs v)
  where

    mzero =
        Codensity (const mzero)

    Codensity m `mplus` Codensity n =
        Codensity $ \k ->
            m k `mplus` n k



toCodensity
    :: Monad m
    => Pattern fs m a
    -> Codensity fs m a
toCodensity f =
    Codensity (f >>=)

fromCodensity
    :: Monad m
    => Codensity fs m a
    -> Pattern fs m a
fromCodensity a =
    runCodensity a return
