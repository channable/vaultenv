# SYNOPSIS
#
#   Git compatible gitignore filter for sources.
#
#   This file exports a single function from the hercules-ci/gitignore package,
#   which implements the correct gitignore behavior (excluding submodules).
#
# USAGE
#
#   let
#     gitignore = import ../path/to/this/file.nix
#   in
#     stdenv.mkDerivation {
#       src = gitignore ./.;
#       ...
#     }
let
  pkgs = import ./nixpkgs.nix {};
  gitignoreRepo = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "ec5dd0536a5e4c3a99c797b86180f7261197c124";
    sha256 = "sha256:0k2r8y21rn4kr5dmddd3906x0733fs3bb8hzfpabkdav3wcy3klv";
  };
  gitignore = import gitignoreRepo { lib = pkgs.lib; };
in
  gitignore.gitignoreSource
