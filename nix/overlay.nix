self: super:
let
  haskellOverlay = import ./haskell-overlay.nix;
in {
  vaultenvHaskellPackages = super.haskell.packages.ghc964.extend haskellOverlay;
}
