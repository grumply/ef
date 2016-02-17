{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Safe #-}
module Ef.Ma where



import Ef.Methods
import Ef.Messages



-- | Ma is the duality between having and using; between existence and
-- interaction. The name comes specifically from Alan Kay's usage.
-- Programming is not done in 'ma' - if one wants the style of dynamic
-- invocation that Kay imagined, one uses the intrinsically dynamic nature of
-- Narratives (conglomerate messages) since they are dynamically generated
-- during sending/execution. In the extreme case, one may use a transformation
-- of Narratives that can be applied before or during sending. In a category
-- theoretic sense, Ma is equivalent to Day convolution.
class Ma f g
    | f -> g
    , g -> f
    where

        ma
            :: (a -> b -> r)
            -> f a
            -> g b
            -> r



instance Ma ((->) a) ((,) a)
    where

        ma use f =
            uncurry (use . f)



instance Ma ((,) a) ((->) a)
    where

        ma use ~(l,r) g =
            use r (g l)



instance Ma (Methods '[]) (Messages '[])



instance (           method              `Ma`            message
         ,  Methods            methods   `Ma`  Messages             messages
         )
     => Ma (Methods (method ': methods))      (Messages (message ': messages))
    where

        ma use (Method method _) (Message message) =
            ma use method message

        ma use (Method _ methods) (Further messages) =
            ma use methods messages
