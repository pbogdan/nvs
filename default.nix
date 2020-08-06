let
  sources = import ./nix/sources.nix;
in
{ pkgs ? import sources.unstable { }
, profiling ? false
}:
let
  hspkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: with pkgs.haskell.lib; {
      ede = overrideCabal hsuper.ede (
        drv: {
          src = pkgs.fetchFromGitHub {
            owner = "brendanhay";
            repo = "ede";
            rev = "5e0373b8a8c83ff2078a938795e30ec8038d228c";
            sha256 = "1lb0q289p6lrc65adlacdx8xy8hrvcywbf6np7rilqdvvnyvlbgs";
          };
        }
      );

      repology-versions = import sources.repology-versions {
        inherit sources;
      };
    };
  };

  inherit (hspkgs)
    callCabal2nix
    ;

  inherit (pkgs.haskell.lib)
    enableLibraryProfiling
    enableExecutableProfiling
    justStaticExecutables
    ;

  inherit (pkgs)
    lib
    nix-gitignore
    ;

  extra-source-excludes = [
    "/.envrc"
    "/shell.nix"
    "/wrapper.nix"
  ];
  nvs = callCabal2nix
    "nvs"
    (nix-gitignore.gitignoreSource extra-source-excludes ./.) { };
in
if profiling then
  enableExecutableProfiling (enableLibraryProfiling nvs)
else
  justStaticExecutables nvs
