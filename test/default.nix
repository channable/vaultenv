{ pkgs ? import ../nix/nixpkgs-pinned.nix {}
}:
  with pkgs; buildEnv {
    name = "vaultenv-testenv";
    paths = [
      cachix
      glibcLocales  # So you can export LOCALE_ARCHIVE=$(nix path-info)/lib/locale/locale-archive.
      perl          # For "prove"
      python3
      stack
      vault
      minikube      # Setting up a kubernetes cluster for the integration tests
      kubectl
    ];
  }
