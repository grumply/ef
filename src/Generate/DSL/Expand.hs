{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Generate.DSL.Expand where

import Generate.Monad
import Generate.Representation
import Generate.Splice
import Generate.Utils

import Data.List
import Data.Maybe
import System.Exit

import qualified Language.Haskell.TH as TH

import Prelude hiding (log)

expand :: String -> TH.Q [TH.Dec]
expand str = return [expandFunSplice str]

expandFunSplice :: String -> TH.Dec
expandFunSplice str =
  TH.FunD
    (TH.mkName "expand")
    [TH.Clause
       []
       (TH.NormalB
          (TH.VarE (TH.mkName str))
       )
       []
    ]

stop :: TH.Q [TH.Dec]
stop = return [expandStopSplice]

expandStopSplice :: TH.Dec
expandStopSplice = TH.FunD (TH.mkName "stop") []

hasExpand :: [TH.Dec] -> Bool
hasExpand [] = False
hasExpand (TH.FunD (TH.nameBase -> "expand") _:_) = True
hasExpand (x:xs) = hasExpand xs

findExpand :: Module -> Mop (String,Int)
findExpand (Module _ _ _ _ _ _ decls) =
  case mapMaybe getExpandSplice decls of
    (x:_) -> return x
    _ -> do log Error "No expand splice found in module decls."
            io exitFailure
  where
    getExpandSplice (SpliceDecl (SrcLoc _ l _) e) =
      case e of
        App (App (Var (UnQual (Ident "mop")))
                  (Con (UnQual (Ident _)))
             )
             (Paren (App (Var (UnQual (Ident "expand")))
                         (Lit (String x))
                    )
             )       -> Just (x,l)
        _            -> Nothing
    getExpandSplice _ = Nothing

findStop :: Module -> Int
findStop (Module _ _ _ _ _ _ decls) =
  let ss = mapMaybe getStopSplice decls
  in if null ss
     then maxBound
     else head ss
  where
    getStopSplice s@(SpliceDecl (SrcLoc _ l _) e) =
      case e of
        Var (UnQual (Ident "stop")) -> Just l
        _                           -> Nothing
    getStopSplice _                  = Nothing


expandSymbols :: Mop [TH.Dec]
expandSymbols = do
  (slc,alg) <- createSymbols
  spliceWith (filter (/= '`')) slc (symbolSetType alg)
  -- writeCosymbols    (createCosymbols    alg)
  -- writeInstructions (createInstructions alg)
  -- writeInterpreters (createInterpreters alg)
  -- writePairings     (createPairings     alg)
  -- renderSymbols alg
  return []

-- create an symbolsic type from a series of functorial instructions.
-- This method assumes that the variables are similarly named. Bypassing
-- this method will be common in the case of a highly variable free variable
-- configuration.
createSymbols :: Mop (SrcLoc,SymbolSet)
createSymbols = do
  MopContext{..} <- ask
  let f = TH.loc_filename location
      m@(Module _ _ _ _ _ _ decls) = originalModule
  (symbolsName,start) <- findExpand m
  let stop = findStop m
      instructions = [ x | x@(DataDecl (SrcLoc _ l _) _ _ _ _ _ _) <- decls
                         , l > start, l < stop ]
      namesAndVars = [ (n,vs) | DataDecl _ _ _ n vs _ _ <- instructions ]
      tyVars = mergeTypeVars $ gatherTypeVars instructions
      lrty = buildInstructionsType location start symbolsName instructions
  if null instructions
  then errorAt "Could not find instructions" start stop
  else do
    -- l <- deleteLine start f
    --log Notify ("Unspliced expand (line " ++ show start ++ "): " ++ show l)
    -- l' <- deleteLine stop f
    -- log Notify ("Unspliced stop (line" ++  show stop ++ "): " ++ show l)

    either
      (\str -> errorAt str start stop >> io exitFailure)
      (\ty@(TypeDecl s _ _ _) -> return $
          (s,SymbolSet (Ident symbolsName) ty instructions)
      )
      lrty

buildInstructionsType :: TH.Loc -> Int -> String -> [Decl] -> Either String Decl
buildInstructionsType TH.Loc{..} srcLoc nm ds =
  let c = mergeContexts     $ gatherContexts     ds
      t = mergeTypeVars     $ gatherTypeVars     ds
      a = mergeInstructions $ gatherInstructions ds
      s = SrcLoc loc_filename srcLoc 0
      n = Ident nm
      y = TyForall Nothing c
  in either Left (\ty -> Right (TypeDecl s n t (y ty))) a

gatherContexts :: [Decl] -> [Context]
gatherContexts ds = [ c | (DataDecl _ _ c _ _ _ _) <- ds ]

mergeContexts :: [Context] -> Context
mergeContexts = nub . concat

gatherTypeVars :: [Decl] -> [[TyVarBind]]
gatherTypeVars ds = [ tyvs | (DataDecl _ _ _ _ (safeInit -> tyvs) _ _) <- ds ]

mergeTypeVars :: [[TyVarBind]] -> [TyVarBind]
mergeTypeVars = nub . concat

gatherInstructions :: [Decl] -> [(Name,[TyVarBind])]
gatherInstructions ds = [ (nm,tyvs) | (DataDecl _ _ _ nm tyvs _ _) <- ds ]

mergeInstructions :: [(Name,[TyVarBind])] -> Either String Type
mergeInstructions [] = Left "Generate.mergeInstructions: empty list"
mergeInstructions (d:ds) =
  let fix l r = TyInfix l (UnQual (Ident ":+:")) r

      build nm tyvs =
        case tyvs of
          [] -> error $ show nm ++ " has no free variables; not a functor."
          xs -> foldr (\a cont st -> cont (TyApp (st (makeVar a))))
                      (\f -> case f undefined of (TyApp l _) -> l)
                      (safeInit xs)
                      (TyApp (TyCon (UnQual nm)))

      makeVar ukv =
        case ukv of
          KindedVar nm _ -> TyVar nm
          UnkindedVar nm -> TyVar nm

      combine nmtys cont fixr = cont $ fix $ fixr $ uncurry build nmtys

      finish f =
        case f undefined of
          TyInfix l _ _ -> l

      start = fix (uncurry build d)

  in Right $ foldr combine finish ds start
