{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Ef.Lang.Vary.Lexemes
    ( Vary(..)
    , get
    , gets
    , put
    , puts
    , swap
    , modify
    , modify'
    ) where



import Ef.Core.Narrative
import Ef.Lang.Vary.Lexicon



get
    :: Say (Vary st) lexicon environment st

get =
    say (Modify id id)



gets
    :: (st -> result)
    -> Say (Vary st) lexicon environment result

gets f =
    say (Modify id f)



put
    :: st
    -> Say (Vary st) lexicon environment ()

put st =
    say (Modify (const st) (const ()))



puts
    :: (a -> st)
    -> a
    -> Say (Vary st) lexicon environment ()

puts f a =
    say (Modify (const (f a)) (const ()))



swap
    :: st
    -> Say (Vary st) lexicon environment st

swap st =
    say (Modify (const st) id)



modify
    :: (st -> st)
    -> Say (Vary st) lexicon environment ()

modify f =
    say (Modify f (const ()))



modify'
    :: (st -> st)
    -> Say (Vary st) lexicon environment ()

modify' f =
    do
      st <- get
      put $! f st



{-# INLINE put #-}
{-# INLINE get #-}
{-# INLINE gets #-}
{-# INLINE puts #-}
{-# INLINE swap #-}
{-# INLINE modify #-}
{-# INLINE modify' #-}
