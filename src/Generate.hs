{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
module Generate where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Maybe
import Language.Haskell.TH

import Product

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

derive :: [Name -> Q [Dec]] -> [Name] -> Q [Dec]
derive qdecs nms =  do
  !decs <- concat <$> mapM (\nm -> concat <$> mapM ($ nm) qdecs) nms
  loc <- location
  let start = fst (loc_start loc)
      end = fst (loc_end loc)
  runIO $ do
    f <- lines <$> readFile (loc_filename loc)
    let strt = take (pred start) f
        deriveLines = map ("-- " ++ ) $ take (end - start) $ drop (pred start) f
        lst = drop end f
    length f `seq`
      writeFile (loc_filename loc)
        $ unlines $ strt ++ deriveLines ++ lines (pprint decs) ++ lst
  runIO (putStrLn $ pprint decs)
  return []

instr :: Name -> Q [Dec]
instr instr_nm = do
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
  -- let sd = StandaloneDerivD [] $
  --          foldl (\st a -> AppT st (VarT a))
  --                (AppT (VarT (mkName "Functor"))
  --                      (VarT nm)
  --                ) $ init params
  return ms


-- interp should generate a coalgebraic representation of a concrete algebra
-- Thus, given:
--
--   data A k = A String k
--   data B k = B String (s -> k)
--
--
interp :: Name -> Q [Dec]
interp instrName@(coName -> coInstrName) = do
  TyConI d <- reify instrName
  (_,params,_,terms0) <- typeInfo (return d)
  let tvs = map PlainTV params
  constructed <-
    if length terms0 > 1
    then makeClosed terms0
    else makeOpen (head terms0)
  let d' = DataD [] coInstrName tvs constructed [mkName "Functor"]
      co_ti = (simpleName $ name d', paramsA d', consA d', termsA d')
  ps <- pairs instrName co_ti
  return $ (d':ps)
  where
    makeOpen (coName -> t,nmts) =
      let listize (AppT (ConT c1) x) = [AppT (ConT c1) x]
          listize (AppT a b) = listize a ++ listize b
          listize (VarT v) = [VarT v]
          listize (ConT v) = [ConT v]
          listize _ = []
          countRes = pred . go
            where
              go (AppT (ConT _) _) = 1
              go (AppT (AppT ArrowT l) r) = go l + go r
              go (AppT l r) = go l + go r
              go (VarT _) = 1
              go (ConT _) = 1
              go _ = 0
          ts = map snd nmts
          res = last ts
          sz = countRes res + 1
          lr = listize res
          coRes = if length lr > 1
                  then foldl AppT (TupleT sz)
                  else head
          converted = if length ts > 1
                      then foldr1 (AppT . AppT ArrowT) (init ts ++ [coRes lr])
                      else coRes lr
      in do runIO (print (ts,res,sz,lr))
            return [ NormalC t [(NotStrict,converted)] ]
    makeClosed ts = do
      open <- mapM makeOpen ts
      return
        [ RecC coInstrName $
            map (\([NormalC _ [(s,conv)]]
                  ,(lowerName . coName -> recNm,_)
                  ) -> (recNm,s,conv)
                )
             $ zip open ts
        ]

pairs :: Name -> TI -> Q [Dec]
pairs instr coinstr_ti = do
  TyConI d <- reify instr
  instr_ti <- typeInfo (return d)
  pairs_ti coinstr_ti instr_ti

pairs_ti :: TI -> TI -> Q [Dec]
pairs_ti co_ti ti = do
  let (nm,params,_,instr_terms) = ti
      (co_nm,co_params,_,_) = co_ti
      isClosed = length instr_terms > 1
  pairings <- if isClosed then createClosedPairing else createOpenPairing
  when (length co_params /= length params) $
    error $ "Variable parameter count must match between Instruction and Interpreter to automatically pair.\nParameters: (instruction,interpreter)\n\t"
            ++ show (params,co_params)
  let co_con = if length co_params <= 1
               then ConT co_nm
               else foldl (\st a -> AppT st (VarT a)) (ConT co_nm) $ init co_params
      con = if length params <= 1
            then ConT nm
            else foldl (\st a -> AppT st (VarT a)) (ConT nm) $ init co_params
      inst_head = ConT (mkName "Pairing") `AppT` co_con `AppT` con
  return [ InstanceD [] inst_head pairings ]
  where
    createOpenPairing = do
      let (co_nm,co_params0,_,_) = co_ti
      let (nm,params0,_,_) = ti
      let rename = newName . nameBase
      params <- mapM rename params0
      co_params <- mapM rename co_params0
      return
       [ FunD (mkName "pair")
           [ Clause
               [ VarP (mkName "p")
               , ConP co_nm [VarP $ head co_params]
               , ConP nm (map VarP params)
               ]
               (NormalB (VarE (mkName "p")
                         `AppE` foldl (\st a -> AppE st (VarE a))
                                      (VarE (head co_params))
                                      (init params)
                         `AppE` (VarE $ last params)
                        )
               )
               []
           ]
       ]
    createClosedPairing = do
      let (co_nm,co_params,co_cons,co_terms) = co_ti
          (nm,params,cons,terms) = ti
          (co_con,co_recs) = head co_terms
      when (length co_cons > 1) $ error $
        "Closed pairing does not yet support interpreter variants.\nIf you have a use for this, please suggest it on:\n\tgithub.com/grumply/mop/issues"
      when (snd (head co_cons) /= length cons) $ error $
        "Closed interpreter expected to have " ++
           show (length cons) ++ " records, but found " ++
           show (snd (head co_cons))
      let recPs = map (fromJust . fst) co_recs
      forM (zip (zip [0 :: Int ..] $ map fst cons) terms) $ \((tc,tcon),(_,ts)) -> do
        ts' <- map fst <$> nameTerms ts
        let ps = reverse $ snd $
                   foldl
                     (\(i,st) a -> if i == tc
                                   then (i+1,VarP a:st)
                                   else (i+1,WildP:st)
                     )
                     (0,[])
                     recPs
        return $
         FunD (mkName "pair")
         [ Clause
             [ VarP (mkName "p")
             , ConP co_nm ps
             , ConP tcon $ map VarP ts'
             ]
             (NormalB $
               (VarE $ mkName "p")
               `AppE`
               foldl AppE
                     (VarE $ recPs !! tc)
                     (init $ map VarE ts')
               `AppE`
               VarE (last ts')

             )
             []
         ]
makeInterpreter :: Name -> Q [Dec]
makeInterpreter nm  = do
  TyConI d <- reify nm
  co_ti@(co_nm,_,co_cons,_) <- typeInfo (return d)
  if snd (head co_cons) > 1
  then makeClosedInterpreter co_ti
  else makeOpenInterpreter co_nm

-- makeOpenInterpreter should, from a Name, generate a coalgebra and
-- an executor
-- Thus, given:
--
--   data CoA k = CoA (String -> k)
--   data CoB s k = CoB (s,k)
--
-- and the coalgebra
--
--   type CoAB s = CoA :*: CoB s
--
-- makeOpenInterpreter should generate
--
--   coAB = coiterT next start
--     where
--       next = coA *:* coB
--       start = Identity id
--
--   coA wa = CoA $ \str -> wa
--
--   coB wa = CoB (undefined,wa)
--
makeOpenInterpreter :: Name -> Q [Dec]
makeOpenInterpreter co_instr_nm = do
  TyConI oi <- reify co_instr_nm
  case oi of
    TySynD _ vars ty -> do
      let vs = map extractVarNames vars
      Just tn <- lookupTypeName ":*:"
      let coalg = filter (/= (ConT tn)) $ gatherCoalgebra vs ty
      mapM makeOpenCoalgebra $ map (\(ConT x) -> x) coalg
    _ -> return . (:[]) =<< makeOpenCoalgebra co_instr_nm

-- makeClosedInterpreter should, from a Name, generate a coalgebra and
-- an executor
-- Thus, given the name of CoAB:
--
--   data CoAB s k = CoAB
--     { coA :: String -> k
--     , coB :: (s,k)
--     }
--
-- makeClosedInterpreter should generate
--
--   coAB = coiterT next start
--     where
--       start = Identity id
--       next = CoAB <$> coA <*> coB
--         where
--           coA wa = \_ -> wa
--           coB wa = (undefined,wa)
--
makeClosedInterpreter :: TI -> Q [Dec]
makeClosedInterpreter co_ti@(co_nm,_,_,co_terms) = do
  let defRet = ConE (mkName "Identity") `AppE` VarE (mkName "id")
      ts = map (VarE . fromJust . fst) . snd $ head co_terms
      coalgebraBuilder = foldl
        (flip UInfixE (VarE $ mkName "<*>"))
        (UInfixE (ConE co_nm) (VarE $ mkName "<$>") (head ts))
        (tail ts)
  coalgebra <- createClosedCoalgebra co_ti
  return
    [ FunD (mkName $ "mk" ++ nameBase co_nm)
       [Clause
         []
         (NormalB $
                  VarE (mkName "coiterT")
           `AppE` VarE (mkName "next")
           `AppE` VarE (mkName "start")
         )
         [ FunD (mkName "start") [Clause [] (NormalB defRet) []]
         , FunD (mkName "next") [Clause [] (NormalB coalgebraBuilder) coalgebra]
         ]
       ]
    ]

createClosedCoalgebra :: TI -> Q [Dec]
createClosedCoalgebra (co_nm,_,_,co_terms) = do
  wa <- newName "wa"
  fmap concat $ forM co_terms $ \(_,ts) ->
    forM ts $ \(Just fun_nm,fun_ty) -> do
      (patterns,body) <- createPatterns wa fun_ty
      return $ FunD fun_nm [ Clause patterns body [] ]
  where
    createPatterns :: Name -> Type -> Q ([Pat],Body)
    createPatterns wa ty = do
      f <- collectFree ty
      let ret = getReturn ty
          n = hasReturn ret
      if isJust n
      then return (VarP wa:map VarP f,NormalB $ buildUndefinedTuple wa (fromJust n))
      else return (VarP wa:map VarP f,NormalB (VarE wa))


extractVarNames (PlainTV n) = n
extractVarNames (KindedTV n _) = n

gatherCoalgebra vs (ConT x)
  | x `elem` vs = []
  | otherwise = [ConT x]
gatherCoalgebra vs (AppT x y) = gatherCoalgebra vs x ++ gatherCoalgebra vs y
gatherCoalgebra _ _ = []

data CoA k = CoA (String -> k)
data CoB k = CoB k
data CoC k = CoC (String,k)
data CoD s k = CoD (s -> (String,k))
type CoAlg s = CoA :*: CoB :*: CoC :*: CoD s

makeOpenCoalgebra :: Name -> Q Dec
makeOpenCoalgebra co_instr_nm = do
  TyConI cod <- reify co_instr_nm
  ti@(co_nm,_,co_cons,co_terms) <- typeInfo (return cod)
  wa <- newName "wa"
  body <- createPatterns (fst $ head co_cons) wa (snd $ last $ snd $ last co_terms)
  return $ FunD (lowerName co_nm) [ Clause [] body [] ]
  where
    createPatterns tc wa ty = do
      f <- collectFree ty
      let ret = getReturn ty
          n = hasReturn ret
      return $ NormalB $ AppE (VarE tc) $ LamE (VarP wa:map VarP f) $
        if isJust n
        then (buildUndefinedTuple wa (fromJust n))
        else (VarE wa)

hasReturn :: Type -> Maybe Int
hasReturn (AppT (TupleT n) _) = Just n
hasReturn (AppT x _) = hasReturn x
hasReturn _ = Nothing

getReturn :: Type -> Type
getReturn (AppT (AppT ArrowT _) x) =
  if isArrowType x
  then getReturn x
  else x
getReturn (AppT _ y) = getReturn y
getReturn (ConT _) = error "Did not expect a higher-kinded return in interpreter."
getReturn t@(VarT _) = t

-- collectFree will extract all non-final variables assuming
-- the correct shape of the interpreter.
collectFree :: Type -> Q [Name]
collectFree (AppT (TupleT _) (VarT x)) = return []
collectFree (AppT x y) = liftM2 (++) (collectFree x) (collectFree y)
collectFree (TupleT _) = do
  nm <- newName "t"
  return [nm]
collectFree (ConT n) = do
  nm <- newName "c"
  return [nm]
collectFree ArrowT = return []
collectFree (VarT v) = return [] -- final var?
collectFree _ = return []

buildUndefinedTuple :: Name -> Int -> Exp
buildUndefinedTuple wa n =
  TupE (replicate (pred n) (VarE (mkName "undefined")) ++ [VarE wa])

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
