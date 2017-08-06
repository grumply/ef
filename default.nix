{ mkDerivation, base, comonad, exceptions, free, kan-extensions
, mmorph, monad-control, mtl, resourcet, stdenv, transformers
, transformers-base
}:
mkDerivation {
  pname = "ef";
  version = "3.0.0.0";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    base comonad exceptions free kan-extensions mmorph monad-control
    mtl resourcet transformers transformers-base
  ];
  homepage = "github.com/grumply/ef";
  description = "Pure, immutable, type-inferred, message-oriented OOP";
  license = stdenv.lib.licenses.bsd3;
}
