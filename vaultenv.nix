{ mkDerivation, async, base, bytestring, connection, containers
, directory, dotenv, hpack, hspec, hspec-discover, hspec-expectations
, http-client, http-conduit, lens, lens-aeson, megaparsec, mtl
, optparse-applicative, parser-combinators, retry, stdenv, text
, unix, unordered-containers, utf8-string, fetchzip
}:

let
  commonDeps = [
    async base bytestring connection containers dotenv http-client
    http-conduit lens lens-aeson megaparsec mtl optparse-applicative
    parser-combinators retry text unix unordered-containers utf8-string
  ];
in
  mkDerivation rec {
    pname = "vaultenv";
    version = "0.10.0";

    src = ./.;

    buildTools = [ hpack ];

    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = commonDeps;
    testHaskellDepends = commonDeps ++ [
      directory hspec hspec-discover hspec-expectations
    ];
    preConfigure = "hpack";
    homepage = "https://github.com/channable/vaultenv#readme";
    description = "Runs processes with secrets from HashiCorp Vault";
    license = stdenv.lib.licenses.bsd3;
    maintainers = with stdenv.lib.maintainers; [ lnl7 ];
    hydraPlatforms = [];
  }
