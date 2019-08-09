let
  nixpkgsRev = "7a53b1cbe79d91167874eb4f0d4ccc69c2e45007";
  nixpkgsTar = fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsRev}.tar.gz";
      sha256 = "sha256:1mwxipvyrwfbl0z6mghv0gr6z61awidnpld1p8h4fwjfifv0dn2j";
    };
  nixpkgsConfig = {
    packageOverrides = pkgsOld: {
      haskellPackages = pkgsOld.haskellPackages.override {
        overrides = haskellNew: haskellOld: {
          dotenv = haskellOld.callPackage ./dotenv.nix {};
        };
      };
    };
  };
in
  # Lambda so we can pass in custom configuration/options to this file later.
  # This seems to be a standard Nix pattern. Currently, we don't take any
  # arguments.
  {}: import "${nixpkgsTar}/default.nix" { config = nixpkgsConfig; }
