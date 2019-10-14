# Pin the version of nixpkgs to the one from `static-haskell-nix`.
# This ensures that we only have a single version of nixpkgs. This
# makes reasoning about the environment a lot easier. We don't want
# to maintain our own nixpkgs distribution with the patches from the
# `static-haskell-nix` project.
#
# Normally, we wouldn't do something like this, but in this instance
# we really want a static binary.
let
  static-haskell-nix = import ./static-haskell-nix.nix;
  pkgs = import "${static-haskell-nix}/nixpkgs.nix";
in
  pkgs
