{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Generate.Splice.Pragma where

import Generate.Monad
import Generate.Splice
import Generate.Utils
import Generate.Cabal

import qualified Language.Haskell.Extension as Ext

type Pragma = SrcLoc -> (ModulePragma,Ext.Extension)

instance TranslationalEq ModulePragma ModulePragma where
  (~==) (LanguagePragma _ ns0)   (LanguagePragma _ ns1)   = ns0  == ns1
  (~==) (OptionsPragma _ mt0 s0) (OptionsPragma _ mt1 s1) = mt0  == mt1 && s0 == s1
  (~==) (AnnModulePragma _ ann0) (AnnModulePragma _ ann1) = ann0 == ann1

instance TranslationalEq a a => TranslationalEq (a,b) a where
  (~==) (a,_) a' = a ~== a'

-- instance TranslationalEq Pragma ModulePragma where
--   (~==) f x = pragma f ~== x

-- instance TranslationalEq ModulePragma Pragma where
--   (~==) x f = x ~== pragma f

instance Locatable ModulePragma where
  locate (LanguagePragma  sl _)   = sl
  locate (OptionsPragma   sl _ _) = sl
  locate (AnnModulePragma sl _)   = sl

  relocate sl (LanguagePragma _ ns)   = LanguagePragma  sl ns
  relocate sl (OptionsPragma _ mt s)  = OptionsPragma   sl mt s
  relocate sl (AnnModulePragma _ ann) = AnnModulePragma sl ann

pragma :: Pragma -> (SrcLoc -> ModulePragma)
pragma slmpe = \sl -> fst (slmpe sl)

extension :: Pragma -> Ext.Extension
extension slmpe = snd (slmpe undefined)

combine :: Ext.Extension -> (SrcLoc -> ModulePragma) -> Pragma
combine e slmp = \sl -> (slmp sl,e)

transitionExtension :: Pragma -> FilePath -> Mop ()
transitionExtension f fp = do
  pm <- io (parseFile fp)
  case pm of
    ParseOk m -> do
      let psl = getPragmaSrcLoc f m
      mapM_ (\sl -> unsplice (srcLine sl) 1 fp) psl
      guaranteeOtherExtension (extension f)
    ParseFailed loc str -> fail $
      "Mop.Generate.Splice.Pragma.transitionExtension: Could not parse module at "
        ++ show loc
        ++ "\nError:\n\t"
        ++ str

getPragmaSrcLoc :: Pragma -> Module -> [SrcLoc]
getPragmaSrcLoc prag (Module _ _ pragmas _ _ _ _) =
  [ locate p | p <- pragmas, prag ~== p ]

findPragmaAppendPoint :: Module -> SrcLoc
findPragmaAppendPoint (Module _ _ pragmas _ _ _ _) =
  lineAfter $ maximum [ locate p | p <- pragmas ]

guaranteeModulePragma :: Pragma -> FilePath -> Mop ()
guaranteeModulePragma p fp = do
  pm <- io (parseFile fp)
  case pm of
    ParseOk m -> unless (hasModulePragma p m)
                        (addModulePragma p $ findPragmaAppendPoint m)
    ParseFailed loc str -> fail $
      "Mop.Generate.Splice.Pragma.guaranteeLocalPragma: Could not parse module at "
        ++ show loc
        ++ "\nError:\n\t"
        ++ str

hasModulePragma :: Pragma -> Module -> Bool
hasModulePragma mp0 (Module _ _ mps _ _ _ _) =
  not $ null [ mp1 | mp1 <- mps, mp1 ~== mp0]

addModulePragma :: Pragma -> SrcLoc -> Mop ()
addModulePragma mp sl = void (splice sl ((pragma mp) sl))

simpleLanguagePragma :: [Name] -> SrcLoc -> ModulePragma
simpleLanguagePragma ns sl = LanguagePragma sl ns

templateHaskellPragma :: Pragma
templateHaskellPragma = combine templateHaskell $ simpleLanguagePragma [Ident "TemplateHaskell"]

quasiQuotePragma :: Pragma
quasiQuotePragma = combine quasiQuotes $ simpleLanguagePragma [Ident "QuasiQuotes"]

typeOperatorsPragma :: Pragma
typeOperatorsPragma = combine typeOperators $ simpleLanguagePragma [Ident "TypeOperators"]
