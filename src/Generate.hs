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
  let tvs = foldr ((:) . PlainTV) [] params
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
  let (nm,_,_,instr_terms) = ti
      (co_nm,_,_,_) = co_ti
      isClosed = length instr_terms > 1
  pairings <- if isClosed then createClosedPairing else createOpenPairing
  let inst_head = ConT (mkName "Pairing") `AppT` ConT co_nm `AppT` ConT nm
  return [ InstanceD [] inst_head pairings ]
  where
    clear = putStrLn ""
    createOpenPairing = do
      let (co_nm,co_params,co_cons,co_terms) = co_ti
      runIO $ do
        print co_nm >> clear
        mapM_ print co_params >> clear
        mapM_ print co_cons >> clear
        mapM_ print co_terms >> clear
      let (nm,params,cons,terms) = ti
      runIO $ do
        print nm >> clear
        mapM_ print params >> clear
        mapM_ print cons >> clear
        mapM_ print terms >> clear
      return []
    createClosedPairing = undefined

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
