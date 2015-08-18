{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Generate.DSL.Helpers.HSE where

import Generate.Monad

import Data.List

import qualified Language.Haskell.TH as TH

varNames :: [Name]
varNames =
  let strings = [c:s | s <- "":strings, c <- ['a'..]]
  in map Ident strings

deduplicateNames :: [Name] -> [Name]
deduplicateNames ns = renameDuplicates ns (findDuplicates ns)
  where
    findDuplicates :: [Name] -> [Name]
    findDuplicates = concat . map snd . filter ((> 1) . fst) . map (\x -> (length x,x)) . groupBy (==)
    renameDuplicates :: [Name] -> [Name] -> [Name]
    renameDuplicates orig [] = orig
    renameDuplicates orig (dup:dups) = renameDuplicates (renameDuplicate orig dup) dups
    renameDuplicate :: [Name] -> Name -> [Name]
    renameDuplicate orig dup = foldr (\a cont (n,dup,acc) -> cont $
                                        if dup == a
                                        then (succ n,dup,rename n a:acc)
                                        else (n,dup,a:acc)
                                     )
                                     (\(_,_,acc) -> acc)
                                     orig
                                     (0,dup,[])
    rename :: Int -> Name -> Name
    rename n (Ident nm) = Ident (nm ++ show n)


convertNm :: Name -> TH.Name
convertNm (Ident nm) = TH.mkName nm

gatherTypeHeads :: [Decl] -> [(Name,[TyVarBind])]
gatherTypeHeads ds = [ (nm,tyvs) | (DataDecl _ _ _ nm tyvs _ _) <- ds ]

gatherTypeVars :: [Decl] -> [[TyVarBind]]
gatherTypeVars ds = [ tyvs | (DataDecl _ _ _ _ tyvs _ _) <- ds ]

gatherContexts :: [Decl] -> [Context]
gatherContexts ds = [ c | (DataDecl _ _ c _ _ _ _) <- ds ]

boundedDecls :: Int -> Int -> [Decl] -> [Decl]
boundedDecls start stop decls =
  [ x | x@(DataDecl (SrcLoc _ l _) _ _ _ _ _ _) <- decls
      , l > start
      , l < stop
      ]

dataNamesAndVars :: [Decl] -> [(Name, [TyVarBind])]
dataNamesAndVars decls =
  [ (n,vs) | DataDecl _ _ _ n vs _ _ <- decls ]

getTypeByName :: String -> [Decl] -> [Decl]
getTypeByName nm decls =
  [ d | d@(TypeDecl _ ((==) (Ident nm) -> True) _ _) <- decls ]


--------------------------------------------------------------------------------
-- HSE patterns ----------------------------------------------------------------
--------------------------------------------------------------------------------
-- Type patterns

pattern TA l r = TyApp l r
pattern TI l c r = TyInfix l c r
pattern TC c = TyCon (UnQual c)
pattern TV nm = TyVar nm

pattern TIL l <- TI l _ _
pattern TIR r <- TI _ _ r

--------------------------------------------------------------------------------
-- Decl patterms

pattern DataName nm <- DataDecl _ _ _ nm _ _ _
pattern DataCons cs <- DataDecl _ _ _ _ _ cs _
pattern TypeType ty <- TypeDecl _ _ _ ty

--------------------------------------------------------------------------------
-- Exp patterns

pattern UI x = UnQual (Ident x)
pattern VUI x = Var (UI x)
pattern PA l r = Paren (App l r)
pattern Str x = Lit (String x)
pattern Sym s = UnQual (Symbol s)
pattern UV nm = UnkindedVar nm

pattern KV nm <- KindedVar nm _
pattern Splice ln e <- SpliceDecl (SrcLoc _ ln _) e

--------------------------------------------------------------------------------
-- HSE Module patterns

pattern Decls ds <- Module _ _ _ _ _ _ ds
pattern Imports is <- Module _ _ _ _ _ is _
pattern Pragmas ps <- Module _ _ ps _ _ _ _
