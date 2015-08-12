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
    ParseOk m -> unless (doesImport slid m) (addImport slid $ findImportAppendPoint m)
    ParseFailed loc str -> fail $
      "Mop.Generate.guaranteeImport: Could not parse module at "
        ++ show loc
        ++ "\nError:\n\t"
        ++ str

doesImport :: (SrcLoc -> ImportDecl) -> Module -> Bool
doesImport (($ undefined) -> ImportDecl _ mn _ _ _ _ _ _)
           (Module _ _ _ _ _ importDecls  _) =
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

findImportAppendPoint :: Module -> SrcLoc
findImportAppendPoint (Module _ _ _ _ _ importDecls _) =
  nextLine $ maximum [ sl | (ImportDecl sl _ _ _ _ _ _ _) <- importDecls ]
  where
    nextLine (SrcLoc fp l c) = SrcLoc fp (succ l) c
