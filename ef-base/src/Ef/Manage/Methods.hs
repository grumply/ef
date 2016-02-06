{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ef.Manage.Methods where


import Ef.Object


data Manage k = Manage Int k k


-- | manage implements the scoping logic for creating
-- managed contexts. Scoped manager logic is implemented
-- in `Ef.Manage`.
manage :: Use Manage methods super
manage =
    Manage 0 pure $ \fs ->
        let Manage i non me = view fs
            i' = succ i
        in i' `seq` pure $ fs .= Manage i' non me
