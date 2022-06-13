{ haskellPackages
, mkDerivation
, lib
, pkgs
, glibcLocales
}:
let
  dependencies = import ./nix/haskell-dependencies.nix haskellPackages;
in
mkDerivation {
  pname = "vaultenv";
  version = "0.15.0";

  src =
    let
      # We do not want to include all files, because that leads to a lot of things that nix
      # has to copy to the temporary build directory that we don't want to have in there
      # (e.g. the `.stack-work` directory, the `.git` directory, etc.)
      prefixWhitelist = builtins.map builtins.toString [
        ./package.yaml
        # Blanket-include for subdirectories
        ./app
        ./src
        ./test
      ];
      # Compute source based on whitelist
      whitelistedSrc = lib.cleanSourceWith {
        src = lib.cleanSource ./.;
        filter = path: _type: lib.any (prefix: lib.hasPrefix prefix path) prefixWhitelist;
      };
      blacklistedSrc = lib.cleanSourceWith {
        src = whitelistedSrc;
        filter = path: type:
          # Where we're going we don't need no documentation
          ! lib.hasSuffix ".md" path;
      };
    in
      blacklistedSrc;

  configureFlags = [
    # Do not print a wall of text before compiling
    "--verbose=0"
    # Treat warnings as errors
    "--ghc-option=-Werror"
  ];

  isLibrary = false;
  isExecutable = true;

  buildTools = [ haskellPackages.hpack glibcLocales ];
  preConfigure = ''
    # Generate the cabal file from package.yaml
    hpack .
  '';

  postInstall = ''
    # By default, a Haskell derivation contains extra stuff that causes the output to retain
    # a dependency on GHC and other build tools. Those are not necessary for running the
    # executable, so we get rid of that.
    rm -rf $out/lib
  '';

  # Disable features that we don't need
  enableLibraryProfiling = false;
  doHoogle = false;
  doHaddock = false;

  # Always run the unit tests
  doCheck = false;
  testToolDepends = [];

  libraryHaskellDepends = dependencies;
  librarySystemDepends = [ glibcLocales ];

  license = lib.licenses.bsd3;
}
