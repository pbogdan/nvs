{ mkDerivation, aeson, aeson-casing, attoparsec, base, bytestring
, containers, directory, ede, lucid, monad-logger
, optparse-applicative, protolude, raw-strings-qq, shell-cmd
, stdenv, temporary, text, unordered-containers, vector
}:
mkDerivation {
  pname = "nix-cve";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing attoparsec base bytestring containers directory
    ede lucid monad-logger optparse-applicative protolude
    raw-strings-qq shell-cmd temporary text unordered-containers vector
  ];
  executableHaskellDepends = [ base protolude ];
  homepage = "https://github.com/pbogdan/nix-cve";
  license = stdenv.lib.licenses.bsd3;
}
