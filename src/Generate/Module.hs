module Generate.Module where

import Generate.Monad

modifyModule :: (Module -> Module) -> Mop ()
modifyModule f =
  modify (\st -> st { currentModule = f $ currentModule st })

modifyModuleName :: (ModuleName -> ModuleName) -> Mop ()
modifyModuleName x =
  modifyModule (\(Module a b c d e f g) -> Module a (x b) c d e f g)

modifyExports :: (Maybe [ExportSpec] -> Maybe [ExportSpec]) -> Mop ()
modifyExports x =
  modifyModule (\(Module a b c d e f g) -> Module a b c d (x e) f g)

modifyImports :: ([ImportDecl] -> [ImportDecl]) -> Mop ()
modifyImports x =
  modifyModule (\(Module a b c d e f g) -> Module a b c d e (x f) g)

modifyPragmas :: ([ModulePragma] -> [ModulePragma]) -> Mop ()
modifyPragmas x =
  modifyModule (\(Module a b c d e f g) -> Module a b (x c) d e f g)

modifyDecls :: ([Decl] -> [Decl]) -> Mop ()
modifyDecls x =
  modifyModule (\(Module a b c d e f g) -> Module a b c d e f (x g))
