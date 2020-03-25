let
  nvs = (import ./default.nix {});
  sources = import ./nix/sources.nix;
  pkgs = (import sources.unstable {});
in
pkgs.haskellPackages.shellFor {
  packages = _: [
    nvs
  ];

  withHoogle = true;

  shellHook = ''
    export HIE_HOOGLE_DATABASE="$(dirname $NIX_GHC)/../share/doc/hoogle/default.hoo";
  '';
}
