{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let shell-cmd-src = (nixpkgs.fetchgit {
  url = "https://github.com/pbogdan/shell-cmd";
  rev = "004e4313c7fca8dbd4dce0993093b4b8d0c56560";
});
in nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./nvs.nix {
  shell-cmd = import (shell-cmd-src) { };
}
