module Generate.DSL.Helpers.TH where

import qualified Generate.Monad as Mop

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH

reify = Mop.liftTH . TH.runQ . TH.reify

-- from the haskell wiki which says its from Syb III/ replib 0.2
typeInfo :: TH.Info -> Mop.Mop (TH.Name, [TH.Name], [(TH.Name, Int)], [(TH.Name, [(Maybe TH.Name, TH.Type)])])
typeInfo (TH.TyConI dec) = Mop.liftTH $ do
  case dec of
    d@(TH.DataD _ _ _ _ _) ->
      return $ (simpleName $ name d, paramsA d, consA d, termsA d)
    d@(TH.NewtypeD _ _ _ _ _) ->
      return $ (simpleName $ name d, paramsA d, consA d, termsA d)
    _ -> error ("derive: not a data type declaration: " ++ show dec)
  where
    consA (TH.DataD _ _ _ cs _)      = map conA cs
    consA (TH.NewtypeD _ _ _ c _)    = [ conA c ]

    paramsA (TH.DataD _ _ ps _ _)    = map nameFromTyVar ps
    paramsA (TH.NewtypeD _ _ ps _ _) = map nameFromTyVar ps

    nameFromTyVar (TH.PlainTV a)     = a
    nameFromTyVar (TH.KindedTV a _)  = a

    termsA (TH.DataD _ _ _ cs _)     = map termA cs
    termsA (TH.NewtypeD _ _ _ c _)   = [ termA c ]

    termA (TH.NormalC c xs)          = (c, map (\x -> (Nothing, snd x)) xs)
    termA (TH.RecC c xs)             = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
    termA (TH.InfixC t1 c t2)        = (c, [(Nothing, snd t1), (Nothing, snd t2)])

    conA (TH.NormalC c xs)           = (simpleName c, length xs)
    conA (TH.RecC c xs)              = (simpleName c, length xs)
    conA (TH.InfixC _ c _)           = (simpleName c, 2)

    name (TH.DataD _ n _ _ _)        = n
    name (TH.NewtypeD _ n _ _ _)     = n
    name d                           = error $ show d

simpleName :: Name -> Name
simpleName nm =
   let s = TH.nameBase nm
   in case dropWhile (/=':') s of
        []          -> TH.mkName s
        _:[]        -> TH.mkName s
        _:t         -> TH.mkName t



--------------------------------------------------------------------------------
-- TH patterns -----------------------------------------------------------------
--------------------------------------------------------------------------------
