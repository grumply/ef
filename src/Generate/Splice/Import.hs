{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generate.Splice.Import where

import Generate.Monad
import Generate.Splice
import Generate.Utils

instance TranslationalEq ImportDecl ImportDecl where
  (~==) (ImportDecl _ mn0 qual0 src0 safe0 pkg0 as0 specs0)
        (ImportDecl _ mn1 qual1 src1 safe1 pkg1 as1 specs1)
    = mn0       == mn1
      && qual0  == qual1
      && src0   == src1
      && safe0  == safe1
      && pkg0   == pkg1
      && as0    == as1
      && specs0 == specs1

guaranteeImport :: (SrcLoc -> ImportDecl) -> FilePath -> Mop ()
guaranteeImport slid fp = do
  pm <- io (parseFile fp)
  case pm of
    ParseOk m -> unless (doesImport slid m) (addImport slid $ findImportAppendPoint m)
    ParseFailed loc str -> fail $
      "Generate.Splice.Import.guaranteeImport: Could not parse module at "
        ++ show loc
        ++ "\nError:\n\t"
        ++ str

doesImport :: (SrcLoc -> ImportDecl) -> Module -> Bool
doesImport iDecl0 (Module _ _ _ _ _ iDecls  _) =
  not $ null [ iDecl1 | iDecl1 <- iDecls, iDecl1 ~== iDecl0]

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
  lineAfter $ maximum [ sl | (ImportDecl sl _ _ _ _ _ _ _) <- importDecls ]
