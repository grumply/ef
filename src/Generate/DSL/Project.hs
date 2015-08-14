{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Generate.DSL.Project where

import Generate.Cabal
import Generate.DSL.Helpers
import Generate.Monad
import Generate.Representation
import Generate.Splice
import Generate.Splice.Import
import Generate.Splice.Pragma
import Generate.Utils

import Control.Arrow

import Data.List
import Data.Maybe
import System.Exit

import qualified Language.Haskell.TH as TH

import Prelude hiding (log)

project :: String -> TH.Q [TH.Dec]
project str = return [expandProjectSplice str]

expandProjectSplice :: String -> TH.Dec
expandProjectSplice str =
  TH.FunD
    (TH.mkName "project")
    [TH.Clause
       []
       (TH.NormalB
          (TH.VarE (TH.mkName str))
       )
       []
    ]

hasProject :: [TH.Dec] -> Bool
hasProject [] = False
hasProject (TH.FunD (TH.nameBase -> "project") _:_) = True
hasProject (_:xs) = hasProject xs

findProject :: Module -> Mop (String,SrcLoc)
findProject (Module _ _ _ _ _ _ decls) =
  case mapMaybe getProjectSplice decls of
    (x:_) -> return x
    _ -> do log error "No project splice found in module decls."
            io exitFailure
  where
    getProjectSplice (SpliceDecl sl@(SrcLoc _ l _) e) =
      case e of
        App (VUI "mop") (PA (VUI "project") (Str x)) -> Just (x,sl)
        _                                            -> Nothing
    getProjectSplice _                                = Nothing

projectSymbols :: Mop [TH.Dec]
projectSymbols = do
  createProjection
  return []

createProjection :: Mop ()
createProjection = do
  MopContext{..} <- ask
  let f = TH.loc_filename location
      m@(Module _ _ _ _ _ imprts decls) = originalModule

  (symbolsName,at) <- findProject m

  let ty = getTypeByName symbolsName decls

  when (null ty) $
      log Error $ "Generate.DSL.Project.createProjection: Could not find " ++ symbolsName

  when (length ty > 1) $
      log Error $ "Generate.DSL.Project.createProjection: Found multiple types for " ++ symbolsName

  symbolNames <- breakSymbolSetDecl (head ty)

  symbols <- gatherSymbols symbolNames decls

  spliced <- sequence $ foldr ((>>>) . (:) . (splice at <=< createSymbol)) id symbols []

  io (print spliced)

  return ()

breakSymbolSetDecl :: Decl -> Mop [Name]
breakSymbolSetDecl ~(TypeType ty) = breakSymbolSetType ty

breakSymbolSetType :: Type -> Mop [Name]
breakSymbolSetType ty = go [] ty
  where
    go acc     (TA x _)                = return $ reverse (getTyCon x:acc)
    go acc     (TC nm)                 = return $ reverse (nm:acc)
    go acc (TI (TA x _) (Sym ":+:") r) = go ((getTyCon x):acc) r
    go acc (TI (TC nm ) (Sym ":+:") r) = go (nm:acc) r
    go (reverse -> acc) ty             = do
      log Error $ "Generate.DSL.Project.breakSymbolSetType:"
                   ++ "\n\tBad type: "        ++ prettyPrint ty
                   ++ "\n\tContinuing with: " ++ unlines (map prettyPrint acc)
      return acc

    getTyCon (TC nm)  = nm
    getTyCon (TA x _) = getTyCon x

-- The problem here is the only way to get constructor information for a symbol
-- is by reifying it through TH, including all sub-symbols.
gatherSymbols :: [Name] -> [Decl] -> Mop [Decl]
gatherSymbols symbols decls = do
  let localSymbols = [ (d,nm) | d@(DataName nm) <- decls
                              , nm `elem` symbols
                              ]
  io (print localSymbols)
  if length localSymbols == length symbols
  then return $ map fst localSymbols
  else do
    nonlocals <- fmap catMaybes $ mapM (convertSymbolInfo <=< reifyHSE)
                                       (map snd localSymbols \\ symbols)
    return (localSymbols ++ nonlocals)

reifyHSE :: Name -> Mop TH.Info
reifyHSE (Ident str)  = liftTH . TH.reify . TH.mkName $ str
reifyHSE (Symbol str) = liftTH . TH.reify . TH.mkName $ str

convertSymbolInfo :: TH.Info -> Mop (Maybe Decl)
convertSymbolInfo (TH.TyConI tci) = do
  io $ putStrLn $ "Generate.DSL.Project.convertSymbolInfo: got TyConI: " ++ show tci
  return Nothing
convertSymbolInfo i = do
  io $ putStrLn $ "Generate.DSL.Project.convertSymbolInfo: expecting TH.TyConI; got: " ++ show i
  return Nothing

createSymbol :: Decl -> Mop Decl
createSymbol d@(DataCons cs) =
  if length cs > 1
  then createClosedSymbol d
  else createOpenSymbol d

createClosedSymbol _ = undefined

createOpenSymbol _ = undefined
