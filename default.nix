{ sources ? (import ./nix/sources.nix)
, compiler ? "ghc883"
, profiling ? false
}:
let
  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages.${compiler}.override {
          overrides = hself: hsuper: with self.haskell.lib; {
            ede = appendPatch hsuper.ede ./patches/ede-0.2.9-ghc882.patch;
            text-format = markUnbroken (doJailbreak hsuper.text-format);
            trifecta =
              appendPatch
                (doJailbreak ((hself.callHackage "trifecta" "2" {})))
                ./patches/trifecta-2-ghc882.patch;
          };
        };
      };
    };
  };

  pkgs = (
    import sources.unstable {
      overlays = [
        overlay
      ];
    }
  );

  inherit (pkgs.haskellPackages)
    callCabal2nix
    shellFor
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
    (nix-gitignore.gitignoreSource extra-source-excludes ./.)
    {};
in
if profiling then
  enableExecutableProfiling (enableLibraryProfiling nvs)
else
  justStaticExecutables nvs
