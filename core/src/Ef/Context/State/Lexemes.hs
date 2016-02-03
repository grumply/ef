{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Context.State.Lexemes
    ( State(..)
    , get
    , gets
    , put
    , puts
    , swap
    , modify
    , modify'
    ) where



import Ef.Core.Narrative
import Ef.Context.State.Lexicon



get
    :: Say (State st) lexicon environment st

get =
    say (Modify id id)



gets
    :: (st -> result)
    -> Say (State st) lexicon environment result

gets f =
    say (Modify id f)



put
    :: st
    -> Say (State st) lexicon environment ()

put st =
    say (Modify (const st) (const ()))



puts
    :: (a -> st)
    -> a
    -> Say (State st) lexicon environment ()

puts f a =
    say (Modify (const (f a)) (const ()))



swap
    :: st
    -> Say (State st) lexicon environment st

swap st =
    say (Modify (const st) id)



modify
    :: (st -> st)
    -> Say (State st) lexicon environment ()

modify f =
    say (Modify f (const ()))



modify'
    :: (st -> st)
    -> Say (State st) lexicon environment ()

modify' f =
    say (Modify (\x -> let x' = f x in x' `seq` x') (const ()))



{-# INLINE put #-}
{-# INLINE get #-}
{-# INLINE gets #-}
{-# INLINE puts #-}
{-# INLINE swap #-}
{-# INLINE modify #-}
{-# INLINE modify' #-}
