# This file wires up the different vaultenv releases that are available.
rec {
  pkgs = import ./nixpkgs-pinned.nix {};

  # Normal cabal build where Nix handles dependencies.
  vaultenv = pkgs.haskellPackages.callPackage ../vaultenv.nix {};
}
