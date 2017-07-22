{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring
      , containers, lucid, protolude, stdenv, text, unordered-containers
      , vector
      }:
      mkDerivation {
        pname = "nix-cve";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson attoparsec base bytestring containers lucid protolude text
          unordered-containers vector
        ];
        homepage = "https://github.com/pbogdan/nix-cve";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
