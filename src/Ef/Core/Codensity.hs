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

newtype Codensity scope parent result =

    Codensity
        { runCodensity
              :: forall b.
                 (result -> Pattern scope parent b)
              -> Pattern scope parent b
        }



instance Functor (Codensity scope parent)
  where

    fmap f (Codensity m) =
        Codensity $ m . (. f)



instance Applicative (Codensity scope parent)
  where

    pure x =
        Codensity ($ x)



    (<*>) =
        ap



instance Monad (Codensity scope parent)
  where

    return x =
        Codensity ($ x)



    m >>= k =
        Codensity $ \c ->

            runCodensity m $ \a ->
                runCodensity (k a) c



instance ( Alternative parent
         , MonadPlus parent
         )
    => Alternative (Codensity scope parent)
  where

    empty =
        Codensity (\_ -> empty)



    Codensity m <|> Codensity n =
        Codensity $ \k ->
            m k <|> n k



instance MonadPlus parent
    => MonadPlus (Codensity scope parent)
  where

    mzero =
        Codensity (const mzero)



    Codensity m `mplus` Codensity n =
        Codensity $ \k ->
            m k `mplus` n k



toCodensity
    :: Monad parent
    => Pattern scope parent result
    -> Codensity scope parent result

toCodensity f =
    Codensity (f >>=)



fromCodensity
    :: Monad parent
    => Codensity scope parent result
    -> Pattern scope parent result

fromCodensity a =
    runCodensity a return
