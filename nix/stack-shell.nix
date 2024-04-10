# This nix expression is used by stack for setting up the build environment.
# It is referenced in `stack.yaml` so that stack uses it by default.
{ }:
let
    nixpkgs = import ./nixpkgs-pinned.nix {};
    getDependencies = import ./haskell-dependencies.nix;
in
    nixpkgs.haskell.lib.buildStackProject {
        name = "vaultenv";
        # This is the GHC that will be seen by stack, and it will come
        # bundled with all the dependencies listed in `haskell-dependencies.nix`.
        # This allows us to have stack use the dependencies from nixpkgs,
        # instead of fetching them itself.
        ghc = nixpkgs.vaultenvHaskellPackages.ghcWithPackages getDependencies;
        buildInputs = with nixpkgs; [
          glibcLocales
        ];
    }
