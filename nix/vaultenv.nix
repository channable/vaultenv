{ haskellPackages
, mkDerivation
, lib
, pkgs
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

  isLibrary = true;
  isExecutable = true;

  doHaddock = false;

  # All these dependencies are concatenated into a global package-db.
  # We don't need to be more granular here.
  executableHaskellDepends = import ./haskell-dependencies.nix haskellPackages;

  license = lib.licenses.bsd3;
}
