{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
module Ef.Lang.Except.Lexemes
     ( Lexicon(..)
     , Throws
     , throwChecked
     , catchChecked
     , tryChecked
     , mapChecked
     ) where



import Ef.Core.Narrative
import Ef.Lang.Except.Lexicon
import qualified Ef.Core.Narrative.Exception as Except

import Control.Exception (Exception(..))

import Data.Coerce
import Data.Proxy



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



throwChecked
    :: ( Excepts lexicon environment
       , Exception e
       )
    => e
    -> (Throws e => Narrative lexicon environment a)

throwChecked e =
    let
        exception =
            toException e

    in
        say (Throw exception undefined)



catchChecked
    :: forall e lexicon environment result.
       ( Excepts lexicon environment
       , Exception e
       )
    => (Throws e => Narrative lexicon environment result)
    -> (e -> Narrative lexicon environment result)
    -> Narrative lexicon environment result

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
    :: ( Excepts lexicon environment
       , Exception e
       )
    => (Throws e => Narrative lexicon environment result)
    -> Narrative lexicon environment (Either e result)

tryChecked a =
    catchChecked (Right <$> a) (return . Left)



mapChecked
    :: ( Excepts lexicon environment
       , Exception e
       , Exception e'
       )
    => (e -> e')
    -> (Throws e => Narrative lexicon environment a)
    -> (Throws e' => Narrative lexicon environment a)

mapChecked f p =
    catchChecked p (throwChecked . f)
