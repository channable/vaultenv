let
  nixpkgsRev = "7a53b1cbe79d91167874eb4f0d4ccc69c2e45007";
  nixpkgsTar = fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsRev}.tar.gz";
      sha256 = "sha256:1mwxipvyrwfbl0z6mghv0gr6z61awidnpld1p8h4fwjfifv0dn2j";
    };
  nixpkgsConfig = {
    allowBroken = true;
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellNew: haskellOld: {
          # Don't run the dotenv test suite. It's broken, and we have our own
          # tests for this.
          dotenv = pkgs.haskell.lib.dontCheck haskellOld.dotenv;
        };
      };
    };
  };
in
  _: import "${nixpkgsTar}/default.nix" { config = nixpkgsConfig; }
