module Generate where

import Language.Haskell.TH

import Data.Char

type TI = (Name, [Name], [(Name, Int)], [(Name, [(Maybe Name, Type)])])

-- Use a data declaration to construct an instruction
make :: Name -> Q [Dec]
make instr_nm = do
  TyConI d <- reify instr_nm
  instr <- typeInfo (return d)
  generateInstruction instr
  where
    generateInstruction :: TI -> Q [Dec]
    generateInstruction
      (name,params,cons,terms) = return $
        flip concatMap cons $ \(con,varCount) ->
          let ln = lowerName con
          in [ (FunD ln
                 [ Clause
                     (foldl (\st a -> VarP a:st) [] params)
                     (NormalB $
                        VarE (mkName "instr")
                        `AppE` (ConE con)
                        `AppE`
                          if varCount == 1
                          then TupE []
                          else
                     )
                     []
                 ])
             ]


lowerName :: Name -> Name
lowerName nm =
   let s = nameBase nm
   in case s of
        ~(up:rest) -> mkName (toLower up:rest)

pairs :: Name -> Name -> Q [Dec]
pairs coinstr instr = do
  TyConI cod@(DataD _ _ _ _ _) <- reify coinstr
  co_ti <- typeInfo (return cod)

  TyConI d@(DataD _ _ _ _ _) <- reify instr
  ti <- typeInfo (return d)

  generatePairingInstance co_ti ti
  where
    generatePairingInstance
      (co_name,[co_param],co_cons,co_terms)
      (name,params,cons,terms) = return $
         [ InstanceD [] (       (ConT $ mkName "Pairing")
                         `AppT` (ConT co_name)
                         `AppT` (ConT name)
                        )
             [ FunD (mkName "pair")
                 [ Clause
                     [ VarP $ mkName "p"
                     , ConP co_name [VarP co_param]
                     , ConP name    (map VarP params)
                     ]
                     (NormalB $
                       (VarE $ mkName "pair")
                       `AppE` (VarE $ mkName "p")
                       `AppE` (foldl
                                (\st a -> st `AppE` (VarE a))
                                (VarE co_param)
                                (init params)
                              )
                       `AppE` (VarE (last params))
                    )
                    []
                 ]
             ]
         ]
    generatePairingInstance _ _ =
      error "Cannot generate pairing for a co-instruction\n\
            \with multiple variables and/or continuations."

typeInfo :: DecQ -> Q (Name, [Name], [(Name, Int)], [(Name, [(Maybe Name, Type)])])
typeInfo m =
     do d <- m
        case d of
           d@(DataD _ _ _ _ _) ->
            return $ (simpleName $ name d, paramsA d, consA d, termsA d)
           d@(NewtypeD _ _ _ _ _) ->
            return $ (simpleName $ name d, paramsA d, consA d, termsA d)
           _ -> error ("derive: not a data type declaration: " ++ show d)

     where
        consA (DataD _ _ _ cs _)    = map conA cs
        consA (NewtypeD _ _ _ c _)  = [ conA c ]

        paramsA (DataD _ _ ps _ _) = map nameFromTyVar ps
        paramsA (NewtypeD _ _ ps _ _) = map nameFromTyVar ps

        nameFromTyVar (PlainTV a) = a
        nameFromTyVar (KindedTV a _) = a

        termsA (DataD _ _ _ cs _) = map termA cs
        termsA (NewtypeD _ _ _ c _) = [ termA c ]

        termA (NormalC c xs)        = (c, map (\x -> (Nothing, snd x)) xs)
        termA (RecC c xs)           = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
        termA (InfixC t1 c t2)      = (c, [(Nothing, snd t1), (Nothing, snd t2)])

        conA (NormalC c xs)         = (simpleName c, length xs)
        conA (RecC c xs)            = (simpleName c, length xs)
        conA (InfixC _ c _)         = (simpleName c, 2)

        name (DataD _ n _ _ _)      = n
        name (NewtypeD _ n _ _ _)   = n
        name d                      = error $ show d

simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        _:[]        -> mkName s
        _:t         -> mkName t
