{ mkDerivation, aeson, aeson-casing, aeson-qq, attoparsec, base
, bytestring, containers, directory, ede, hashable, hspec, lucid
, monad-logger, optparse-applicative, protolude, QuickCheck
, quickcheck-instances, raw-strings-qq, shell-cmd, stdenv
, temporary, text, transformers, unordered-containers, vector
, versions, yaml
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
    unordered-containers vector versions yaml
  ];
  executableHaskellDepends = [ base protolude ];
  testHaskellDepends = [
    aeson aeson-qq base containers hspec protolude QuickCheck
    quickcheck-instances text unordered-containers vector
  ];
  homepage = "https://github.com/pbogdan/nvs";
  license = stdenv.lib.licenses.bsd3;
}
