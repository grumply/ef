{-# LANGUAGE ViewPatterns #-}
module Generate where

import Language.Haskell.TH

import Data.Char

type Params = [Name]
type Constructors = [(Name,Int)]
type Terms = [(Name,[(Maybe Name,Type)])]
type TI = (Name,Params,Constructors,Terms)

nameTerms :: [(Maybe Name,Type)] -> Q [(Name,(Maybe Name,Type))]
nameTerms ts = do
  let varNames = [ c : s | s <- "":varNames, c <- ['a'..'z'] ]
      prenamedTerms = zip varNames ts
  mapM (\(nm,t) -> newName nm >>= \n -> return (n,t)) prenamedTerms

lowerName :: Name -> Name
lowerName nm =
   let s = nameBase nm
   in case s of
        ~(up:rest) -> mkName (toLower up:rest)

coName :: Name -> Name
coName = mkName . ("Co" ++) . nameBase

make :: Name -> Q [Dec]
make instr_nm = do
  TyConI d <- reify instr_nm
  (_,params,_,terms) <- typeInfo (return d)
  mapM (generateInstruction params) terms
  where
    generateInstruction [] _ = error
      "A continuation parameter must exist - like k in the following:\n\n\
      \  F k = A Int k\n\
      \      | A (Bool -> k)"
    generateInstruction params (con@(lowerName -> ln),terms0) = do
      runIO (print (params,con,terms0))
      let cont = last params
      terms1 <- nameTerms terms0
      let (arrTy,hasCont) = if null terms1
                            then (False,False)
                            else check cont (last terms1)
          terms = if hasCont then init terms1 else terms1
          varsP = map (VarP . fst) terms
          varsE = map (VarE . fst) terms
          body = createInstruction arrTy hasCont con varsE
      return $ FunD ln [ Clause varsP body [] ]

    check cont (_,(_,ty)) = (isArrowType ty,searchType cont ty)
      where
        isArrowType (AppT ArrowT _) = True
        isArrowType (AppT _ ArrowT) = True
        isArrowType (AppT ty1 ty2) = isArrowType ty1 || isArrowType ty2
        isArrowType _ = False

        searchType c (AppT _ t) = searchType c t
        searchType c         t  = t == VarT c

    normal con ps
      = NormalB $ AppE (VarE (mkName "liftF"))
      $ ParensE $ AppE (VarE (mkName "inj"))
                $ foldl AppE (ConE con) ps

    createInstruction _     False con vars = normal con vars
    createInstruction False _     con vars = normal con (vars ++ [TupE []])
    createInstruction True  _     con vars = normal con (vars ++ [VarE (mkName "id")])


makeCo :: Name -> Q [Dec]
makeCo instrName@(coName -> coInstrName) = do
  TyConI d <- reify instrName
  ti <- typeInfo (return d)
  runIO $ print ti
  return []

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
