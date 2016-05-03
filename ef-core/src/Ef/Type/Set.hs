{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Safe #-}
module Ef.Type.Set where


type family (:+:) a xs
  where

    a :+: '[] =
        '[a]

    a :+: a ': xs =
        (a ': xs)

    a :+: x ': xs =
        x ': a :+: xs


type xs ∪ ys = Union xs ys


type family Union xs ys
  where

    Union '[] ys =
        ys

    Union (x ': xs) ys =
        x :+: (xs ∪ ys)



type family In (x :: k) (xs :: [k]) :: Bool
  where

    In x '[] =
        'False

    In x (x ': xs) =
        'True

    In x (y ': xs) =
        In x xs



type family (/==) (x :: k) (y :: k) :: Bool
  where

    (/==) x x =
        'False

    (/==) x y =
        'True


class Denies (x :: k) (ys :: [k])



instance Denies x '[]



instance ( (/==) x y ~ 'True
         , Denies x ys
         )
    => Denies x (y ': ys)
