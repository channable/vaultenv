let
  # `master` on 2020-02-02.
  rev = "c360f2a15f6947b411ecbd7ebaea925f6dbd68df";
  tarball = fetchTarball {
    url = "https://github.com/nh2/static-haskell-nix/archive/${rev}.tar.gz";
    sha256 = "sha256:0y6ppiagh6dbvdhhnrq572xnw2yzn6d0gcmajrfdgdfwhsl21g95";
  };
in
  # The logic we care about lives in this Nix file.
  import "${tarball}/survey/default.nix"
