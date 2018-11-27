{ pkgs ? import <nixpkgs> {}, compiler ? "ghc844" }:
(import ./default.nix { inherit pkgs compiler; }).env
