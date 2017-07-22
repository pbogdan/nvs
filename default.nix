{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let shell-cmd-src = (nixpkgs.fetchgit {
  url = "https://github.com/pbogdan/shell-cmd";
  rev = "24f058f9ba188aef127940004b63222db359443e";
});
in nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./nix-cve.nix {
  shell-cmd = import (shell-cmd-src) { };
}
