{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let shell-cmd-src = (nixpkgs.fetchgit {
  url = "https://github.com/pbogdan/shell-cmd";
  rev = "d6c8435286edaa451a34afe1d5a7940a377e643f";
  sha256 = "1y1hid7zs8kkxszgsqdd577fxfa0z812b1i3lbyn5lrnsdzg8kc9";
});
in nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./nvs.nix {
  shell-cmd = import (shell-cmd-src) { inherit nixpkgs compiler; };
}
