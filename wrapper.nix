let
  nvs = (import ./default.nix {});
  sources = (import ./nix/sources.nix);
  pkgs = (import sources.unstable {});

  inherit (pkgs)
    writeScriptBin
    ;
  inherit (pkgs.lib)
    concatMapStringsSep
    range
    ;
  feeds = builtins.map
    (year: ./feeds + "/nvdcve-1.1-${toString year}.json")
    (range 2002 2020);
in
writeScriptBin "nvs" ''
  #!${pkgs.runtimeShell}

  ${nvs}/bin/nvs \
      ${concatMapStringsSep " \\\n    " (feed: "--nvd-feed " + feed) feeds} \
      $@
''
