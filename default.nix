{ pkgs ? import ./nix/nixpkgs-pinned.nix {}
}:
  with pkgs; buildEnv {
    name = "vaultenv-devenv";
    paths = [
      cachix
      niv
      perl # For "prove"
      python3
      stack
      vault
    ];
  }
