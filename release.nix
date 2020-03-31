{ pkgs ? import nix/nixpkgs.nix {}
}:

{
  vaultenv = pkgs.haskellPackages.callPackage ./nix/vaultenv.nix {};
}
