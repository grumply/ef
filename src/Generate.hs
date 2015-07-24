{-# LANGUAGE ViewPatterns #-}
module Generate where

import Control.Monad
import Data.Char

import Language.Haskell.TH

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

isArrowType :: Type -> Bool
isArrowType (AppT ArrowT _) = True
isArrowType (AppT _ ArrowT) = True
isArrowType (AppT ty1 ty2) = isArrowType ty1 || isArrowType ty2
isArrowType _ = False

unqualify :: Name -> Name
unqualify = mkName . reverse . takeWhile (/= '.') . reverse . nameBase

typeEndsIn :: Type -> Type -> Bool
typeEndsIn cont (AppT _ t) = typeEndsIn cont t
typeEndsIn cont t  = t == cont

noContinuation =
  "A continuation parameter must exist or the algebra will not compose.\n\
  \Instructions should have a continuation type as their last variable,\n\
  \like 'k' in the following:\n\n\
  \    F k = A Int k\n\
  \        | A (Bool -> k)"

embed :: Q [Dec] -> Q ()
embed qdecs =  do
  decs <- qdecs
  loc <- location
  let start = fst (loc_start loc)
      end = fst (loc_end loc)
  runIO $ do
    f <- lines <$> readFile (loc_filename loc)
    length f `seq`
      writeFile (loc_filename loc)
        $ unlines
        $ replace decs end (start,end) f
  where
    replace :: [Dec] -> Int -> (Int,Int) -> [String] -> [String]
    replace decs e = go
      where
        go (0,0) fs = (++) (lines $ pprint decs) ("-- End auto-generation.":fs)
        go (0,(==) e -> True) fs = "-- Start auto-generation.":go (0,e-1) fs
        go (0,n) (f:fs) = ("-- " ++ f):go (0,n-1) fs
        go (st,e') (f:fs) = f:go (st-1,e') fs

make :: Name -> Q [Dec]
make instr_nm = do
  TyConI d <- reify instr_nm
  (nm,params,_,terms0) <- typeInfo (return d)
  when (null params) $ error noContinuation
  let cont = VarT $ last params
  ms <- forM terms0 $ \(con@(lowerName -> ln),terms1) -> do
    terms2 <- nameTerms terms1
    let finTy = snd $ snd $ last terms2
        arrTy    = not (null terms2) && isArrowType finTy
        hasCont  = arrTy && typeEndsIn cont finTy
        unitOrId = if arrTy   then VarE (mkName "id") else TupE []
        terms    = if hasCont then init terms2        else terms2
        end      = [unitOrId | hasCont]
    return $ FunD ln
      [ flip (Clause $ map (VarP . fst) terms) [] $
          NormalB     $ AppE (VarE (mkName "liftF"))
            $ ParensE $ AppE (VarE (mkName "inj"))
            $ foldl     AppE (ConE $ unqualify con)
            $ map (VarE . fst) terms ++ end
      ]
  let sd = StandaloneDerivD [] $
           foldl (\st a -> AppT st (VarT a))
                 (AppT (VarT (mkName "Functor"))
                       (VarT nm)
                 ) $ init params
  return (sd:ms)

makeCo :: Name -> Q [Dec]
makeCo instrName@(coName -> coInstrName) = do
  TyConI d <- reify instrName
  (_,params,_,terms0) <- typeInfo (return d)
  cont <- newName "k"
  constructed <-
    if isClosed terms0
    then makeClosed params terms0
    else makeOpen params terms0
  return
    [ DataD [] coInstrName
            (foldl (\st a -> (PlainTV a):st) [] params)
            []
            [mkName "Functor",mkName "Generic"]
    ]
  where
    isClosed = (> 1) . length
    makeClosed ps ts = return (TupE [])
    makeOpen ps ts = return (TupE [])

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
