{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Generate.DSL.Expand where

import Generate.Cabal
import Generate.DSL.Helpers
import Generate.Monad
import Generate.Representation
import Generate.Splice
import Generate.Splice.Import
import Generate.Splice.Pragma
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
  createSymbols
  -- writeCosymbols    (createCosymbols    alg)
  -- writeInstructions (createInstructions alg)
  -- writeInterpreters (createInterpreters alg)
  -- writePairings     (createPairings     alg)
  -- renderSymbols alg
  return []

createSymbols :: Mop ()
createSymbols = do
  MopContext{..} <- ask
  let f = TH.loc_filename location
      m@(Module _ _ _ _ _ _ decls) = originalModule
  (symbolsName,start) <- findExpand m
  let stop         = findStop m
      instructions = boundedDecls start stop decls
      lrty         = buildInstructionsType location stop symbolsName instructions
  guaranteeImport mopSymbols f
  transitionExtension typeOperatorsPragma f
  if null instructions
  then errorAt "Could not find instructions" start stop
  else do
    deleteLine start f
    when (stop /= maxBound) $ void $ deleteLine stop f
    case lrty of
      Left str -> errorAt str start (if stop == maxBound then (-1) else stop)
      Right ty@(TypeDecl s _ _ _) -> void $ spliceWith (filter (/= '`')) s ty

buildInstructionsType :: TH.Loc -> Int -> String -> [Decl] -> Either String Decl
buildInstructionsType TH.Loc{..} srcLoc nm ds =
  let c = nub . concat $ gatherContexts  ds
      t = nub . concat . map safeInit $ gatherTypeVars ds
      a = symbolSumFromTypeHeads $ gatherTypeHeads ds
      s = SrcLoc loc_filename srcLoc 0
      n = Ident nm
      y = TyForall Nothing c
  in either Left (\ty -> Right (TypeDecl s n t (y ty))) a

symbolSumFromTypeHeads :: [(Name,[TyVarBind])] -> Either String Type
symbolSumFromTypeHeads [] = Left "Generate.DSL.Expand.symbolSumFromTypeHeads: empty list"
symbolSumFromTypeHeads (d:ds) =
  let fixl l r = TyInfix l (UnQual (Ident ":+:")) r

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

      combine nmtys cont fixr = cont $ fixl $ fixr $ uncurry build nmtys

      finish f =
        case f undefined of
          TyInfix l _ _ -> l

      start = fixl (uncurry build d)

  in Right $ foldr combine finish ds start
