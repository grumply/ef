{-# LANGUAGE ViewPatterns #-}
module Generate.Splice.Import
  (guaranteeImport
  ,doesImport
  ,addImport
  ,simpleImport
  ,mopTape,mopComputer,mopInstructions,mopSymbols
  ,findImportAppendPoint
  ) where

import Generate.Monad
import Generate.Splice

guaranteeImport :: (SrcLoc -> ImportDecl) -> FilePath -> Mop ()
guaranteeImport slid fp = do
  pm <- io (parseFile fp)
  case pm of
    ParseOk m@(Module _ _ _ _ _ importDecls _) ->
      unless (doesImport slid importDecls)
             (addImport slid $ findImportAppendPoint importDecls)
    ParseFailed loc str -> fail $
      "Mop.Generate.guaranteeImport: Could not parse module at "
        ++ show loc
        ++ "\nError:\n\t"
        ++ str

doesImport :: (SrcLoc -> ImportDecl) -> [ImportDecl] -> Bool
doesImport (($ undefined) -> ImportDecl _ mn _ _ _ _ _ _) importDecls =
  not $ null $
    [ x | x@(ImportDecl _ mn' _ _ _ _ _ _) <- importDecls
        , mn' == mn
        ]

addImport :: (SrcLoc -> ImportDecl) -> SrcLoc -> Mop ()
addImport f sl = void (splice sl (f sl))

simpleImport :: ModuleName -> SrcLoc -> ImportDecl
simpleImport mn sl = ImportDecl sl mn False False False Nothing Nothing Nothing

mopTape,mopComputer,mopInstructions,mopSymbols :: SrcLoc -> ImportDecl
mopTape         = simpleImport (ModuleName "Mop.Tape")
mopComputer     = simpleImport (ModuleName "Mop.Computer")
mopInstructions = simpleImport (ModuleName "Mop.Instructions")
mopSymbols      = simpleImport (ModuleName "Mop.Symbols")

findImportAppendPoint :: [ImportDecl] -> SrcLoc
findImportAppendPoint [] = error
  "findImportAppendPoint: Cannot determine append point from empty list of import declarations."
findImportAppendPoint decls =
  nextLine $ maximum [ sl | (ImportDecl sl _ _ _ _ _ _ _) <- decls ]
  where
    nextLine (SrcLoc fp l c) = SrcLoc fp (succ l) c
