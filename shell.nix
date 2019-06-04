{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cereal, HUnit, QuickCheck
      , stdenv
      }:
      mkDerivation {
        pname = "describe";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base bytestring cereal ];
        testHaskellDepends = [ base bytestring cereal HUnit QuickCheck ];
        homepage = "https://github.com/riugabachi/describe";
        description = "Combinators for describing binary data structures in a manner that facilitates implicit derivation of serialization/deserialization monads";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
