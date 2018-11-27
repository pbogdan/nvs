{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:
let config = {
    packageOverrides = super: let self = super.pkgs; in
    {
        haskell = super.haskell // {
            packageOverrides = self: super: {
                streaming-utils = pkgs.haskell.lib.doJailbreak super.streaming-utils;
            };
    };
    };
};
  shell-cmd-src = (pkgs.fetchgit {
    url = "https://github.com/pbogdan/shell-cmd";
    rev = "3e06c7888b0d30396bd022d2ee851b50a89f69b4";
    sha256 = "1z7sai6ayff941kmqnc33db7b0qjr4x9fdp68r3d4pamyfd9ps1p";
  });
  nixpkgs = import <nixpkgs> { inherit config; };
in
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./nvs.nix {
  shell-cmd = import (shell-cmd-src) { inherit nixpkgs compiler; };
}
