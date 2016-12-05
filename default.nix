{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, cereal, cmdargs
      , containers, directory, HUnit, mtl, process, stdenv
      , test-framework, test-framework-hunit
      }:
      mkDerivation {
        pname = "python-pickle";
        version = "0.2.2";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          attoparsec base bytestring cereal containers mtl
        ];
        executableHaskellDepends = [ base bytestring cmdargs ];
        testHaskellDepends = [
          base bytestring containers directory HUnit process test-framework
          test-framework-hunit
        ];
        description = "Serialization/deserialization using Python Pickle format";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
