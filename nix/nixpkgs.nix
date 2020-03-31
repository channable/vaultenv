let
  # Nixpkgs unstable on 2020-02-02. This is the same Nixpkgs as the one that
  # static-haskell-nix uses. We use the same one to get cache hits.
  rev = "0c960262d159d3a884dadc3d4e4b131557dad116";
  tarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:0d7ms4dxbxvd6f8zrgymr6njvka54fppph1mrjjlcan7y0dhi5rb";
  };
in
  import tarball
