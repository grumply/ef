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



instance Uses Exceptable attrs parent
    => Binary (Attribute Exceptable attrs parent)
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
       , Is Excepting scope parent
       ) => e -> (Throws e => Pattern scope parent a)

throwChecked e =
    self (Throw (toException e) undefined)



catchChecked
    :: forall e scope parent result.
       ( Exception e
       , Is Excepting scope parent
       )
    => (Throws e => Pattern scope parent result)
    -> (e -> Pattern scope parent result)
    -> Pattern scope parent result

catchChecked act =
    let
      proxy =
          Proxy :: Proxy e

    in
      Except.catch (unthrow proxy act)
  where
    unthrow
        :: forall proxy e x.
           proxy e
        -> (Throws e => x) -> x

    unthrow _ = unWrap . coerceWrap . Wrap



    coerceWrap
        :: forall e x.
           Wrap e x
        -> Wrap (Catch e) x

    coerceWrap = coerce



tryChecked
    :: forall e scope parent result.
       ( Is Excepting scope parent
       , Exception e
       )
    => (Throws e => Pattern scope parent result)
    -> Pattern scope parent (Either e result)

tryChecked a =
    catchChecked (Right <$> a) (return . Left)



excepter
    :: Attribute Exceptable attrs x

excepter =
    let
      uncaught err =
          "Uncaught exception: " ++ (show err)

    in
      Exceptable (error . uncaught)



mapChecked
    :: ( Is Excepting scope parent
       , Exception e
       , Exception e'
       )
    => (e -> e')
    -> (Throws e => Pattern scope parent a)
    -> (Throws e' => Pattern scope parent a)

mapChecked f p =
    catchChecked p (throwChecked . f)

-- | Inlines

{-# INLINE excepter #-}
{-# INLINE throwChecked #-}
{-# INLINE catchChecked #-}
{-# INLINE tryChecked #-}
{-# INLINE mapChecked #-}
