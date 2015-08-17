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
      m@(Module _ _ _ _ _ _ decls) = originalModule

  (symbolsName,at) <- findProject m

  let ty = getTypeByName symbolsName decls

  when (null ty) $
      log Error $ "Generate.DSL.Project.createProjection: Could not find " ++ symbolsName

  when (length ty > 1) $
      log Error $ "Generate.DSL.Project.createProjection: Found multiple types for " ++ symbolsName

  symbolNames <- breakSymbolSet (head ty)

  symbols <- gatherSymbols at symbolNames

  sequence_ (foldr ((>>>) . (:) . splice at) id symbols [])

  return ()

breakSymbolSet :: Decl -> Mop [Name]
breakSymbolSet ~(TypeType ty) = go [] ty
  where
    go acc     (TA x _)                = return $ reverse (getTyCon x:acc)
    go acc     (TC nm)                 = return $ reverse (nm:acc)
    go acc (TI (TA x _) (Sym ":+:") r) = go ((getTyCon x):acc) r
    go acc (TI (TC nm ) (Sym ":+:") r) = go (nm:acc) r
    go (reverse -> acc) ty'            = do
      log Error $ "Generate.DSL.Project.breakSymbolSetType:"
                   ++ "\n\tBad type: "        ++ prettyPrint ty'
                   ++ "\n\tContinuing with: " ++ unlines (map prettyPrint acc)
      return acc

    getTyCon (TC nm)  = nm
    getTyCon (TA x _) = getTyCon x

gatherSymbols :: SrcLoc -> [Name] -> Mop [Decl]
gatherSymbols at = fmap concat <$> mapM (synthesize at <=< typeInfo <=< reify) . map convertNm

synthesize :: SrcLoc -> (TH.Name,[TH.Name],[(TH.Name,Int)],[(TH.Name,[(Maybe TH.Name,TH.Type)])]) -> Mop [Decl]
synthesize sl (nm,params,cons,terms) = do
  let symbolNm = Ident (uncapitalize (TH.nameBase nm))
  -- let rhs = App (Var (UnQual (Ident "liftF")))
  --               (Paren (App (Var (UnQual (Ident "inj")))
  --                           (Paren buildSymbol)
  --                      )
  --               )
  --     buildSymbol = foldr (\a cont st -> cont (App (st (mkVar a))))
  --                         (\res -> if isFun
  --                                  then res (Var (UnQual (Ident "id")))
  --                                  else res (Var (Special UnitCon)))
  --                         (safeInit params)
  --                         (App (Con (UnQual (Ident (TH.nameBase nm)))))
  --     mkVar = Var . UnQual . Ident . TH.nameBase
  --     mkMatch nm _ =
  --       [Match sl (Ident (uncapitalize (TH.nameBase nm)))
  --                 (map makeVar (safeInit undefined))
  --                 Nothing
  --                 (UnGuardedRhs rhs)
  --                 (BDecls [])
  --       ]
  -- return $ map (FunBind . uncurry mkMatch) terms
  flip mapM terms $ \(con,ts) -> do
    io (print (con,ts))
    return undefined


isFun = const True

makeVar :: TH.Name -> Pat
makeVar (TH.nameBase -> nm) = PVar (Ident nm)
