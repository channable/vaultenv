let
  pkgs = import ./nix/nixpkgs.nix {};
  ghc = pkgs.haskellPackages.ghcWithPackages (import ./nix/haskell-dependencies.nix);
in
  pkgs.buildEnv {
    name = "vaultenv-devenv";
    paths = [
      ghc
      pkgs.cabal-install
      pkgs.cachix
      pkgs.vault
    ];
  }

