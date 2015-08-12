{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Generate.DSL.Expand where

import Generate.DSL.Helpers
import Generate.Monad
import Generate.Representation
import Generate.Splice
import Generate.Splice.Import
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
      namesAndVars = dataNamesAndVars instructions
      lrty         = buildInstructionsType location stop symbolsName instructions
  guaranteeImport mopSymbols f
  if null instructions
  then errorAt "Could not find instructions" start stop
  else do
    l <- deleteLine start f
    log Notify ("Unspliced expand in " ++ f ++ " (line " ++ show start ++ "):\n\t " ++ l)
    l' <- deleteLine stop f

    log Notify ("Unspliced stop in " ++ f ++ " (line" ++  show stop ++ "):\n\t " ++ l')

    case lrty of
      Left str -> errorAt str start stop
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
symbolSumFromTypeHeads [] = Left "Generate.DSL.Expand.symbolSetFromTypeHeads: empty list"
symbolSumFromTypeHeads (d:ds) =
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
