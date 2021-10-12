{ pkgs ? (import ../../nix/nixpkgs-pinned.nix) {} }:

let
  vaultenv = (import ../../nix/release.nix).vaultenv;

  container = pkgs.dockerTools.buildLayeredImage {
    name = "vaultenv/vaultenv";
    tag = "test";
    contents = [
      vaultenv
      pkgs.coreutils
    ];
  };

in
  container
