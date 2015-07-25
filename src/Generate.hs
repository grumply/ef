{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Maybe
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
  let tvs = map PlainTV params
  constructed <-
    if length terms0 > 1
    then makeClosed terms0
    else makeOpen (head terms0)
  return
    [ DataD [] coInstrName tvs constructed [mkName "Functor"] ]
  where
    makeOpen (coName -> t,nmts) =
      let listize (AppT a b) = listize a ++ listize b
          listize (VarT v) = [VarT v]
          listize (ConT v) = [ConT v]
          listize _ = []
          countRes = pred . go
            where
              go (AppT (AppT ArrowT l) r) = 1 + go l + go r
              go (AppT l r) = go l + go r
              go (VarT _) = 1
              go (ConT _) = 1
              go _ = 0
          ts = map snd nmts
          res = last ts
          sz = countRes res
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


pairs :: Name -> Name -> Q [Dec]
pairs co_instr instr = do
  TyConI cod <- reify co_instr
  coinstr_ti <- typeInfo (return cod)
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
      let (co_nm,co_params,_,_) = co_ti
      let (nm,params,_,_) = ti
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
makeInterpreter co_instr_nm = do
  TyConI cod <- reify co_instr_nm
  ti@((co_nm,_,co_cons,co_terms) :: TI) <- typeInfo (return cod)
  if snd (head co_cons) > 1
  then makeClosedInterpreter ti
  else makeOpenInterpreter ti
  where
   makeClosedInterpreter :: TI -> Q [Dec]
   makeClosedInterpreter ti@(co_nm,_,co_cons,co_terms) = do
     let defRet = ConE (mkName "Identity") `AppE` VarE (mkName "id")
         ts = map (VarE . fromJust . fst) . snd $ head co_terms
         coalgebraBuilder = foldl
           (flip UInfixE (VarE $ mkName "<*>"))
           (UInfixE (ConE co_nm) (VarE $ mkName "<$>") (head ts))
           (tail ts)
     coalgebra <- makeOpenInterpreter ti
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
   makeOpenInterpreter :: TI -> Q [Dec]
   makeOpenInterpreter (co_nm,_,co_cons,co_terms) = do
     wa <- newName "wa"
     runIO $ do
       print co_cons
       print co_terms
     let defRet = ConE (mkName "Identity") `AppE` VarE (mkName "id")
         ts = map (VarE . fromJust . fst) . snd $ head co_terms
         coalgebraBuilder = foldl
           (flip UInfixE (VarE $ mkName "<*>"))
           (UInfixE (ConE co_nm) (VarE $ mkName "<$>") (head ts))
           (tail ts)
         ct = snd $ last $ snd $ last co_terms
     runIO (print ct)
     if length (snd $ head co_terms) > 1
     then return $ flip map (snd $ head co_terms) $ \(Just nm,ty) ->
              FunD
                 nm
                 [ Clause
                 ([VarP wa] ++ (convertToPat $ last $ collectVars ct))
                 (NormalB $ makeEmptyFunction wa ct)
                 []
             ]

     else return $
         [ FunD
             (mkName $ "mk" ++ nameBase co_nm)
             [ Clause
                 []
                 (NormalB $
                          VarE (mkName "coiterT")
                   `AppE` VarE (mkName "next")
                   `AppE` VarE (mkName "start")
                 )
                 [ FunD (mkName "start") [Clause [] (NormalB defRet) []]
                 , FunD (mkName "next") [Clause [] (NormalB coalgebraBuilder) []]
                 ]
             ]
         , FunD
             (lowerName co_nm)
             [ Clause
                 ([VarP wa] ++ (convertToPat $ last $ collectVars ct))
                 (NormalB $ makeEmptyFunction wa ct)
                 []
             ]
         ]

convertToPat :: Type -> [Pat]
convertToPat (AppT a b) = convertToPat a ++ convertToPat b
convertToPat (VarT v) = [VarP v]
convertToPat (ConT v) = [ConP v []]
convertToPat _ = []

makeEmptyFunction :: Name -> Type -> Exp
makeEmptyFunction em (pred . countVars -> n) = TupE $ (replicate n (VarE (mkName "undefined"))) ++ [VarE em]

collectVars :: Type -> [Type]
collectVars = init . go
  where
    go (AppT t0 t1) = go t0 ++ go t1
    go vt@(VarT _) = [vt]
    go _ = []

countVars (AppT (TupE _) x) = countVars x
countVars (AppT ArrowT (VarT _)) = 0
countVars (AppT ArrowT (ConT _)) = 0
countVars (AppT (AppT (TupleT _) _) x) = 1 + countVars x
countVars (AppT t0 t1) = countVars t0 + countVars t1
countVars (VarT _) = 1
countVars (ConT _) = 1
countVars _ = 0

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
