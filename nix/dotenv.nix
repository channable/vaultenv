# Patched version of the dotenv derivation from Nixpkgs from the following loc:
# NixOS/nixpkgs/master/pkgs/development/haskell-modules/hackage-packages.nix
# at commit `f964c1468ee68b17ce0682c17a49d3bcd0143270`
{ mkDerivation, base, base-compat, containers, directory
, exceptions, hspec, hspec-megaparsec, megaparsec
, optparse-applicative, process, text, transformers, yaml
, stdenv
}:
let
  githubRev = "4a51c475032bfe4af5da3bc361a364412e14914e";
in
  mkDerivation {
    pname = "dotenv";
    version = "0.8.0.2";

    src = fetchTarball {
      url = "https://github.com/stackbuilders/dotenv-hs/archive/${githubRev}.tar.gz";
      sha256 = "sha256:0vr4rinzh9fik7jgzaixk6j9wk8s950y5mcg2pl6fjqszmig34i0";
    };

    isLibrary = true;
    isExecutable = true;

    enableSeparateDataOutput = true;

    libraryHaskellDepends = [
      base base-compat containers directory exceptions megaparsec process
      text transformers yaml
    ];
    executableHaskellDepends = [
      base base-compat megaparsec optparse-applicative process text
      transformers yaml
    ];
    testHaskellDepends = [
      base base-compat containers directory exceptions hspec
      hspec-megaparsec megaparsec process text transformers yaml
    ];
    description = "Loads environment variables from dotenv files";
    license = stdenv.lib.licenses.mit;
  }
