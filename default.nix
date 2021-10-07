{ pkgs ? import ./nix/nixpkgs-pinned.nix {}
}:
  with pkgs; buildEnv {
    name = "vaultenv-devenv";
    paths = [
      cachix
      glibcLocales  # So you can export LOCALE_ARCHIVE=$(nix path-info)/lib/locale/locale-archive.
      niv
      perl # For "prove"
      python3
    ];
  }
