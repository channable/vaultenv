# Pin static-haskell-nix version.
let
  static-haskell-nix-rev = "ff7715e0e13fb3f615e64a8d8c2e43faa4429b0f";
  static-haskell-nix = builtins.fetchTarball {
    url = "https://github.com/nh2/static-haskell-nix/archive/${static-haskell-nix-rev}.tar.gz";
    sha256 = "sha256:17ir87i7sah9nixvh25qhzh19bqv3vgnfg4nfy4wv631q4gfj7fb";
  };
in
  static-haskell-nix
