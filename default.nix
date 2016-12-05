{ mkDerivation, attoparsec, base, bytestring, cereal, cmdargs
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
}
