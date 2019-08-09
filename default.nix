let
  pkgs = import ./nix/nixpkgs.nix {};
in
  pkgs.haskellPackages.callPackage ./vaultenv.nix {}
