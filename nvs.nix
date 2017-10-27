{ mkDerivation, aeson, aeson-casing, aeson-qq, base, bytestring
, containers, directory, ede, hashable, hspec, json-stream, lucid
, monad-logger, optparse-applicative, protolude, QuickCheck
, quickcheck-instances, shell-cmd, stdenv, streaming
, streaming-bytestring, streaming-utils, temporary, text, time
, transformers, unordered-containers, vector, versions, yaml
}:
mkDerivation {
  pname = "nvs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base bytestring containers directory ede
    hashable json-stream lucid monad-logger optparse-applicative
    protolude shell-cmd streaming streaming-bytestring streaming-utils
    temporary text time transformers unordered-containers vector
    versions yaml
  ];
  executableHaskellDepends = [ base protolude ];
  testHaskellDepends = [
    aeson aeson-qq base containers hspec protolude QuickCheck
    quickcheck-instances text unordered-containers vector
  ];
  homepage = "https://github.com/pbogdan/nvs";
  license = stdenv.lib.licenses.bsd3;
}
