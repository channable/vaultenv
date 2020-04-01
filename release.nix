# This file wires up the different vaultenv releases that are available.
let
  pkgs = import ./nix/nixpkgs.nix {};

  static = import ./nix/static-haskell-nix.nix {
    integer-simple = true;
    approach = "pkgsMusl";
  };

  static-gmp = import ./nix/static-haskell-nix.nix {
    integer-simple = false;
    approach = "pkgsMusl";
  };
in
  {
    # Normal cabal build where Nix handles dependencies.
    vaultenv = pkgs.haskellPackages.callPackage ./nix/vaultenv.nix {};

    # NB: For both of these packages, we pass arguments explicitly instead
    # of using `callPackage`. `callPackage` from `static-haskell-nix` does not
    # appear to work.

    # There is still something wrong with this derivation. I get errors in
    # the tests of the TLS library.
    vaultenv-static = (import ./nix/vaultenv.nix {
      haskellPackages = static.haskellPackages;
      static = true;
      lib = static.lib;
      pkgs = static.pkgs;
      mkDerivation = static.haskellPackages.mkDerivation;
      Cabal = static.haskellPackages.Cabal;
    });

    # Static binary including GMP. We cannot distribute this because vaultenv is
    # not LGPL licensed. We can build it for ourselves though. We want this,
    # because `integer-gmp` is a lot faster to use for TLS connections than
    # `integer-simple`. This is useful when you start vaultenv a lot. Most users
    # shouldn't need this.
    vaultenv-static-gmp = (import ./nix/vaultenv.nix {
      haskellPackages = static-gmp.haskellPackages;
      static = true;
      lib = static-gmp.lib;
      pkgs = static-gmp.pkgs;
      mkDerivation = static-gmp.haskellPackages.mkDerivation;

      # We need to explicitly pass the Cabal version from static-haskell-nix
      # because it includes some relevant patches.
      Cabal = static-gmp.haskellPackages.Cabal;
    });

  }

