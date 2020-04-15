{ haskellPackages
, mkDerivation
, lib
, pkgs
, Cabal
, static ? false
}:

mkDerivation {
  pname = "vaultenv";
  version = "0.13.1";

  src =
    lib.cleanSourceWith {
      filter = path: type:
        (lib.hasSuffix ".cabal" path || lib.hasSuffix ".hs" path || lib.hasSuffix ".secrets" path || type == "directory") &&
        lib.cleanSourceFilter path type;
      src = ../.;
    };

  # Explicitly depend a version of Cabal the library. Otherwise we can't
  # pick up the right version when building the static library.
  setupHaskellDepends = [ Cabal ];

  # All these dependencies are concatenated into a global package-db.
  # We don't need to be more granular here.
  executableHaskellDepends = import ./haskell-dependencies.nix haskellPackages;

  # We just want the executable. We're not interested in all the other
  # stuff.
  isExecutable = true;
  enableSharedExecutables = false;
  enableLibraryProfiling = false;
  isLibrary = false;
  doHaddock = false;
  postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";

  configureFlags = if static then [
    # This flag requires we use the Cabal version from static-haskell-nix.
    # It's patched in and sets some required GHC options. The regular cabal
    # + static linking options for GHC don't work together with MUSL.
    "--enable-executable-static"

    # Unused when building against integer-simple.
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"

    "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
  ] else [];

  license = lib.licenses.bsd3;
}
