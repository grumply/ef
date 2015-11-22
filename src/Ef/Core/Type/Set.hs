{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Ef.Core.Type.Set where



type family (:+:) a xs
  where

    a :+: '[] =
        '[a]

    a :+: (a ': xs) =
        (a ': xs)

    a :+: (x ': xs) =
        x ': a :+: xs



type family (∪) {- emacs: c-x 8 <enter> 222a <enter> -} xs ys
  where

    '[] ∪ ys =
        ys

    (x ': xs) ∪ ys =
        x :+: (xs ∪ ys)



type family In (x :: k) (xs :: [k]) :: Bool
  where

    In x '[] =
        'False

    In x (x ': xs) =
        'True

    In x (y ': xs) =
        In x xs



class Subset xs ys



instance Subset '[] ys



instance ( In x ys ~ 'True
         , Subset xs ys
         )
    => Subset (x ': xs) ys



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
