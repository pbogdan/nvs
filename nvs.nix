{ mkDerivation, aeson, aeson-casing, attoparsec, base, bytestring
, containers, directory, ede, hashable, lucid, monad-logger
, optparse-applicative, protolude, raw-strings-qq, shell-cmd
, stdenv, temporary, text, unordered-containers, vector, yaml
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
    raw-strings-qq shell-cmd temporary text unordered-containers vector
    yaml
  ];
  executableHaskellDepends = [ base protolude ];
  homepage = "https://github.com/pbogdan/nixpkgs-vuln-scanner";
  license = stdenv.lib.licenses.bsd3;
}
