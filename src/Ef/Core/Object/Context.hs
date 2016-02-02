{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ef.Core.Object.Context
    ( Context(..)
    , Does'(..)
    , Does(..)
    , stretch
    , Extend(..)
    ) where



import Ef.Core.Type.Set
import Ef.Core.Type.Nat


import Data.Binary
import Data.Typeable
import Unsafe.Coerce



data Context (attrs :: [* -> *]) x
  where

    Empty
        :: Context '[] x

    Context
        :: Denies attr attrs
        => attr x
        -> Context attrs x
        -> Context (attr ': attrs) x



instance Eq (Context '[] x) where
    _ == _ = True



instance (Eq (context x),Eq (Context contexts x))
        => Eq (Context (context ': contexts) x)
    where

        (Context attr0 contexts0) == (Context attr1 contexts1) =
            attr0 == attr1 && contexts0 == contexts1



instance Ord (Context '[] x) where
    _ <= _ = True



instance (Ord (context x),Ord (Context contexts x))
        => Ord (Context (context ': contexts) x)
    where

        (Context attr0 contexts0) <= (Context attr1 contexts1) =
            attr0 <= attr1 && contexts0 <= contexts1



instance Binary (Context '[] x)
  where

    get =
       pure Empty



    put _ =
       pure ()



instance ( Binary (Context attrs x)
         , Denies attr attrs
         , Binary (attr x)
         )
    => Binary (Context (attr ': attrs) x)
  where

    get =
        Context <$> get <*> get



    put (Context x as) =
        put x >> put as



instance Functor (Context '[])
  where

    fmap _ _ =
        Empty



instance ( Functor attr
         , Functor (Context attrs)
         )
    => Functor (Context (attr ': attrs))
  where

    fmap f (Context attr attrs) =
        Context
            (fmap f attr)
            (fmap f attrs)



instance {-# OVERLAPPING #-} Show (Context '[] methods)
    where

        show _ = ""



instance {-# OVERLAPS #-} 
         ( attrs ~ (attr ': attrs')
         , Typeable (attr ())
         , Show (attr method)
         , Show (Context attrs' method)
         )
         => Show (Context attrs method)
    where

        show (Context attr attrs) =
            let
                attrType =
                    typeOf (undefined :: attr ())

                attrString =
                    reverse . (" :" ++) . drop 3 . reverse . show $ attrType
            in
               case attrs of
                 
                   Empty -> 
                       attrString ++ show attr
                       
                   _ ->
                       attrString ++ show attr ++ ", " ++ show attrs


class Does (attr :: * -> *) (attrs :: [* -> *])
  where

    push
        :: attr x
        -> Context attrs x
        -> Context attrs x



    pull
        :: Context attrs x
        -> attr x



instance ( index ~ IndexOf attr attrs
         , Does' attr attrs index
         )
    => Does attr attrs
  where

    push attr =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          push' index attr



    pull attrs =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          pull' index attrs



stretch
    :: ( index ~ IndexOf attr attrs
       , Does' attr attrs index
       )
    => (forall z. attr z -> attr z)
    -> Context attrs x
    -> Context attrs x
stretch f attrs =
    let
      attr =
          pull attrs

    in
      push (f attr) attrs



class Does' (attr :: * -> *) (attrs :: [* -> *]) (n :: Nat)
  where

    push'
        :: Index n
        -> attr x
        -> Context attrs x
        -> Context attrs x



    pull'
        :: Index n
        -> Context attrs x
        -> attr x



instance attrs ~ (attr ': xs)
    => Does' attr attrs 'Z
  where

    push' _ attr (Context _ attrs) =
        Context attr attrs



    pull' _ (Context attr _) =
        attr



instance ( index ~ IndexOf attr attrs
         , Does' attr attrs index
         )
    => Does' attr (attr' ': attrs) ('S n)
  where

    push' _ attr (Context attr' attrs) =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          Context attr' (push' index attr attrs)



    pull' _ (Context _ attrs) =
        let
          index =
              Index :: Index (IndexOf attr attrs)

        in
          pull' index attrs


-- Is this actually useful since the morphisms require certain types?
class Extend (small :: [* -> *]) (large :: [* -> *])
  where

    extend
        :: Context small x
        -> Context large x
        -> Context large x



    implements
        :: Context large x
        -> Context small x



instance Extend '[] largeContext
  where

    extend _ large =
        large



    implements _ =
        Empty



instance ( Denies attr small
         , index ~ IndexOf attr large
         , Does' attr large index
         , Extend small large
         )
    => Extend (attr ': small) large
  where

    extend (Context attr small) large =
        extend small (push attr large)



    implements large =
        Context (pull large) (implements large)
