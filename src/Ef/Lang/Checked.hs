{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Ef.Lang.Checked
  ( Excepting
  , excepter
  , Exceptable
  , throwChecked
  , catchChecked
  , tryChecked
  , mapChecked
  , Exception(..)
  , SomeException(..)
  ) where



import Ef.Core
import qualified Ef.Core.Pattern.Exception as Except

import Control.Exception (SomeException(..),Exception(..),assert)
import Data.Binary
import Data.Coerce
import Data.Proxy



data Excepting k
  where

    Throw
        :: SomeException
        -> k
        -> Excepting k



class Throws e



type role Throws representational



newtype Catch e = Catch e



instance Throws (Catch e)



newtype Wrap e a =
    Wrap
        {
          unWrap
              :: Throws e => a
        }



data Exceptable k
  where

    Exceptable
        :: (    SomeException
             -> k
           )
        -> Exceptable k



instance Uses Exceptable gs m
    => Binary (Attribute Exceptable gs m)
  where

    get =
        pure excepter



    put _ =
        pure ()



instance Exceptable `Witnessing` Excepting
  where

    witness use (Exceptable k) (Throw e k') =
        use (k e) k'



throwChecked
    :: ( Exception e
       , Is Excepting fs m
       ) => e -> (Throws e => Pattern fs m a)

throwChecked e =
    self (Throw (toException e) undefined)



catchChecked
    :: forall e fs m a.
       ( Exception e
       , Is Excepting fs m
       )
    => (Throws e => Pattern fs m a)
    -> (e -> Pattern fs m a)
    -> Pattern fs m a

catchChecked act =
    let
      proxy =
          Proxy :: Proxy e

    in
      Except.catch (unthrow proxy act)
  where
    unthrow
        :: forall proxy e a.
           proxy e
        -> (Throws e => a) -> a

    unthrow _ = unWrap . coerceWrap . Wrap



    coerceWrap
        :: forall e a.
           Wrap e a
        -> Wrap (Catch e) a

    coerceWrap = coerce



tryChecked
    :: forall a b fs m .
       ( Is Excepting fs m
       , Exception a
       )
    => (Throws a => Pattern fs m b)
    -> Pattern fs m (Either a b)

tryChecked a =
    catchChecked (Right <$> a) (return . Left)



excepter
    :: Attribute Exceptable gs k

excepter =
    let
      uncaught err =
          "Uncaught exception: " ++ (show err)

    in
      Exceptable (error . uncaught)



mapChecked
    :: ( Is Excepting fs m
       , Exception e
       , Exception e'
       )
    => (e -> e')
    -> (Throws e => Pattern fs m a)
    -> (Throws e' => Pattern fs m a)

mapChecked f p =
    catchChecked p (throwChecked . f)

-- | Inlines

{-# INLINE excepter #-}
{-# INLINE throwChecked #-}
{-# INLINE catchChecked #-}
{-# INLINE tryChecked #-}
{-# INLINE mapChecked #-}
