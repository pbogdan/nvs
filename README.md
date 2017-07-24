# nixpkgs vulnerability scanner

nixpkgs-vuln-scanner (nvs) is a tool working against a local [nixpkgs](https://github.com/NixOS/nixpkgs) checkout to scan for potentially present vulnerabilities published in [National Vulnerability Database](https://nvd.nist.gov/).

nvs is in an early stage of development, as such the results it produces may not be 100% accurate. As it internally relies on `nix-env` to query the available packages is it subject to its limitations.

## Installation

Requires [Nix](https://nixos.org/nix/) package manager:

```
$ git clone https://github.com/pbogdan/nixpkgs-vuln-scanner
$ cd nixpkgs-vuln-scanner
$ nix-build
$ nix-env -i ./result
```

## Usage

### Prerequisites

nvsrequires:

1. A local nixpkgs checkout:

    ```
    $ git clone https://github.com/NixOS/nixpkgs
    # switch to the desired branch as required
    ```
2. A copy of the JSON feed published by NVD, the feed can be obtained from https://nvd.nist.gov/vuln/data-feeds#JSON_FEED

### Using nvs

Available command line options:

```
$ nvs --help
Usage: nvs --nvd-feed nvd-feed --nixpkgs nixpkgs [--markdown] file
  Experimental CVE scanner for nixpkgs

Available options:
  -h,--help                Show this help text
  --nvd-feed nvd-feed      Path to a copy of the NVD JSON feed
  --nixpkgs nixpkgs        Path to nixpkgs checkout
  --markdown               render markdown instead of HTML
  file                     Output path for the generated report
```

Example invocation:

```
$ nvs --nvd-feed /home/pbogdan/nvdcve-1.0-2017.json --nixpkgs /home/pbogdan/nixpkgs report.html
```

will produce an HTML report in the current directory. Alternatively the `--markdown` switch produces a format more suitable for usage in GitHub issues.
