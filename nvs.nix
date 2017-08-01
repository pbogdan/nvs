{ mkDerivation, aeson, aeson-casing, attoparsec, base, bytestring
, containers, directory, ede, hashable, hspec, lucid, monad-logger
, optparse-applicative, protolude, QuickCheck, quickcheck-instances
, raw-strings-qq, shell-cmd, stdenv, temporary, text, transformers
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "nvs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing attoparsec base bytestring containers directory
    ede hashable lucid monad-logger optparse-applicative protolude
    raw-strings-qq shell-cmd temporary text transformers
    unordered-containers vector yaml
  ];
  executableHaskellDepends = [ base protolude ];
  testHaskellDepends = [
    base containers hspec protolude QuickCheck quickcheck-instances
    text unordered-containers vector
  ];
  homepage = "https://github.com/pbogdan/nixpkgs-vuln-scanner";
  license = stdenv.lib.licenses.bsd3;
}
