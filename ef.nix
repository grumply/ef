{ mkDerivation, base, exceptions, free, kan-extensions, mmorph
, monad-control, mtl, resourcet, stdenv, transformers
, transformers-base, ghc, trivial, pipes
}:
mkDerivation rec {
  pname = "ef";
  version = "3.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions free kan-extensions mmorph monad-control mtl
    resourcet transformers transformers-base
  ];
  testDepends = libraryHaskellDepends ++ [ trivial ];
  benchmarkDepends = libraryHaskellDepends ++ [ trivial pipes ];
  homepage = "github.com/grumply/ef";
  license = stdenv.lib.licenses.bsd3;
}
