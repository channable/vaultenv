self: super:
let
  haskellOverlay = import ./haskell-overlay.nix;
in {
  Ghc902Packages = super.haskell.packages.ghc902.extend haskellOverlay;
}
