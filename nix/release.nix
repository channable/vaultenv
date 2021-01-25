let
  pkgs = import ./nixpkgs-pinned.nix {};
in {
  # Normal cabal build where Nix handles dependencies.
  vaultenv = pkgs.haskellPackages.callPackage ../vaultenv.nix {};
}
