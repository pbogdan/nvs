# nixpkgs vulnerability scanner

nvs is a tool working against a local [nixpkgs](https://github.com/NixOS/nixpkgs) checkout to scan for potentially present vulnerabilities published in [National Vulnerability Database](https://nvd.nist.gov/).

nvs is in an early stage of development, as such the results it produces may not be 100% accurate. As it internally relies on `nix-env` to query the available packages is it subject to its limitations.

## Installation

Requires [Nix](https://nixos.org/nix/) package manager:

```
$ git clone https://github.com/pbogdan/nvs
$ cd nvs
$ nix-build
$ nix-env -i ./result
```

## Usage

### Prerequisites

nvs requires:

1. A local nixpkgs checkout:

    ```
    $ git clone https://github.com/NixOS/nixpkgs
    # switch to the desired branch as required
    ```
2. A copy of the JSON feed published by NVD, the feed can be obtained from https://nvd.nist.gov/vuln/data-feeds#JSON_FEED

### Using nvs

Available command line options:

```
Usage: nvs --nvd-feed nvd-feed --nixpkgs nixpkgs (--html | --markdown | --json)
           [--cpe] [--verbose]
  Experimental CVE scanner for nixpkgs

Available options:
  -h,--help                Show this help text
  --nvd-feed nvd-feed      Path to a copy of the NVD JSON feed. May be specified
                           multiple times.
  --nixpkgs nixpkgs        Path to nixpkgs checkout.
  --html                   Render HTML.
  --markdown               Render Markdown.
  --json                   Render JSON.
  --cpe                    use CPE matching mode.
  --verbose                Verbose output.
```

Example invocation:

```
$ nvs --nvd-feed /home/pbogdan/nvdcve-1.0-2017.json --nixpkgs /home/pbogdan/nixpkgs --html
```

will produce an HTML report on stdout.

#### Managing vulnerabilites exclusions

If for any reason you want to exclude a particular vulnerability for being considered when generating the report please add it to `data/excludes.yaml` file. Once you modified the file and wish to preview the changes you must either:

- invoke `nvs` from your checkout of nvs
- rebuild & reinstall `nvs`.

`nvs` looks for the file either relative to the current working directory, otherwise if not found it will use the version bundled with the installed package.

#### Managing package aliases

`nvs` can be made aware of package name aliases via `data/aliases.yml` file. This is useful when a package in nixpkgs collection figures in a different name in a vulnerabilities source. For example, as mentioned in "Known issues" section `vlc` package in nixpkgs collection is present in NVD as `vlc_media_player`. To preview the change refer to the [comments in the previous section](#managing-vulnerabilites-exclusions).

## Known issues

- there might be mismatches in product names and package names in packages. For example nixpkgs `vlc` package is present in NVD as `vlc_media_player`. I don't see a way of handling this automatically. Currently this is handled via manually curated package alias database.

## Related projects

- [nixpkgs-monitor](https://github.com/Phreedom/nixpkgs-monitor)
- [vulnix](https://github.com/flyingcircusio/vulnix)

