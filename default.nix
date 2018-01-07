{ mkDerivation, base, comonad, exceptions, free, kan-extensions
, mmorph, monad-control, mtl, pipes, resourcet, stdenv
, transformers, transformers-base, trivial
}:
mkDerivation {
  pname = "ef";
  version = "3.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base comonad exceptions free kan-extensions mmorph monad-control
    mtl resourcet transformers transformers-base
  ];
  testHaskellDepends = [
    base comonad exceptions free kan-extensions mmorph monad-control
    mtl resourcet transformers transformers-base trivial
  ];
  benchmarkHaskellDepends = [
    base comonad exceptions free kan-extensions mmorph monad-control
    mtl pipes resourcet transformers transformers-base trivial
  ];
  homepage = "github.com/grumply/ef";
  license = stdenv.lib.licenses.bsd3;
}
