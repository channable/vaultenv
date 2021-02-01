{ pkgs ? import ./nix/nixpkgs-pinned.nix {}
}:
  with pkgs; buildEnv {
    name = "vaultenv-devenv";
    paths = [
      stack
      vault
      cachix
      niv
    ];
  }
