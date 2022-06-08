let
  pkgs = import ./nixpkgs-pinned.nix {};
in {
  # Normal cabal build where Nix handles dependencies.
  vaultenv = pkgs.haskellPackages.callPackage ../vaultenv.nix {};

  # Static package build
  vaultenvStatic = (pkgs.pkgsStatic.haskellPackages.callPackage ../vaultenv.nix {
    # Use non-static version of glibc locales, as the static version fails to build.
    glibcLocales = pkgs.glibcLocales;
  }).overrideAttrs (prev: {
    # Disable the check phase, as hspec-discover will not run.
    # TODO: investigate what's happening with hspec-discover.
    doCheck = false;

    # Remove the fixupPhase, as for some reason it still tries to strip the statically built
    # executable.
    # TODO: Cleaner way of disabling the fixup phase.
    fixupPhase = " ";
  });
}
