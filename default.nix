{ compiler ? "ghc801" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          extension = new: old: {
            cabal = pkgs.haskell.packages.cabalNoTest;
          };
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = new: old: rec {

              ef =
                new.callPackage ./ef.nix { };

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { ef = pkgs.haskell.packages.${compiler}.ef;
  }

