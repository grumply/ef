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

  io (print symbols)

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
gatherSymbols at = fmap concat <$> mapM (synthesize at <=< typeInfo <=< reify)
                 . map convertNm

synthesize :: SrcLoc -> THInfo -> Mop [Decl]
synthesize sl THInfo{..} = do
  flip mapM infoTerms $ \(con,map snd -> ts) -> do
    let vs = safeInit (map smartName ts)
        buildSymbol = foldr (\nm cont st -> cont (App (st (Var (UnQual nm)))))
                            (\res -> if null ts
                                     then case res undefined of App l _ -> l
                                     else if isHigherKinded (last ts)
                                          then res (Var (UnQual (Ident "id")))
                                          else res (Var (Special UnitCon))
                            )
                            vs
                            (App (Con (UnQual (Ident (TH.nameBase con)))))
    return (FunBind
              [Match
                 sl
                 (Ident (uncapitalize (TH.nameBase con)))
                 (map PVar vs)
                 Nothing
                 (UnGuardedRhs (App (Var (UnQual (Ident "liftF")))
                                    (Paren (App (Var (UnQual (Ident "inj")))
                                                (Paren buildSymbol)
                                           )
                                    )
                               )
                 )
                 (BDecls [])
              ]
           )

isHigherKinded (TH.AppT _ _) = True
isHigherKinded _ = False

varNames :: [Name]
varNames =
  let strings = [c:s | s <- "":strings, c <- ['a'..]]
  in map Ident strings

smartName :: TH.Type -> Name
smartName = Ident . go []
  where
    go acc (TH.AppT l r) =
      let accl = go [] l
          accr = go [] r
      in acc ++ accl ++ accr
    go acc (TH.VarT nm) = acc ++ (TH.nameBase nm)
    go acc (TH.ConT nm) = acc ++ (uncapitalize (TH.nameBase nm))
    go acc _ = acc

makeVar :: TH.Name -> Pat
makeVar (TH.nameBase -> nm) = PVar (Ident nm)
