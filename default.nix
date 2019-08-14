let
  pkgs = (import ./nix/nixpkgs.nix {}).pkgsMusl;
in
  pkgs.haskellPackages.callPackage ./vaultenv.nix {libcurl = pkgs.curl;}
