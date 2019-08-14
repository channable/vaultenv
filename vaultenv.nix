{ mkDerivation, async, base, bytestring, containers
, directory, dotenv, hspec, hspec-discover, hspec-expectations
, http-client, http-client-openssl, lens, lens-aeson, megaparsec, mtl
, optparse-applicative, parser-combinators, retry, stdenv, text
, unix, unordered-containers, utf8-string, fetchzip, pkgs
}:

let
  commonDeps = [
    async base bytestring containers dotenv http-client
    http-client-openssl lens lens-aeson megaparsec mtl optparse-applicative
    parser-combinators retry text unix unordered-containers utf8-string
  ];
  gitignore = import ./nix/gitignore.nix;
in
  mkDerivation rec {
    pname = "vaultenv";
    version = "0.10.0";

    src = gitignore ./.;

    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = commonDeps;
    testHaskellDepends = commonDeps ++ [
      directory hspec hspec-discover hspec-expectations
    ];

    # Enable static linking by passing extra libs from Nixpkgs.
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ];

    homepage = "https://github.com/channable/vaultenv#readme";
    description = "Runs processes with secrets from HashiCorp Vault";
    license = stdenv.lib.licenses.bsd3;
  }
