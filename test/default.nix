{ pkgs ? import ../nix/nixpkgs-pinned.nix
  # Allow vault to be used only as a part of this testing environment shell.
  # We are not allowing the use of vault as a part of our final package
  # because vault-1.16.1 is lisenced under BSL-1.1
  { config.allowUnfreePredicate = pkg:
      (pkgs.lib.getName pkg) == "vault" &&
      (pkgs.lib.getVersion pkg) == "1.16.1";
  }
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
