{ compiler ? "ghc801" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = new: old: rec {

              trivial =
                new.callPackage ./deps/trivial/trivial.nix { };

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
    trivial = pkgs.haskell.packages.${compiler}.trivial;
  }

