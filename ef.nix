{ mkDerivation, base, comonad, exceptions, free, kan-extensions
, mmorph, monad-control, mtl, resourcet, stdenv, transformers
, transformers-base
}:
mkDerivation {
  pname = "ef";
  version = "3.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base comonad exceptions free kan-extensions mmorph monad-control
    mtl resourcet transformers transformers-base
  ];
  homepage = "github.com/grumply/ef";
  license = stdenv.lib.licenses.bsd3;
}
