let
  pkgs = import ./nix/nixpkgs.nix;
in
  pkgs.buildEnv {
    name = "vaultenv-devenv";
    paths = [
      pkgs.stack
      pkgs.vault
    ];
  }

