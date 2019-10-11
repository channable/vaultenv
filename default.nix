# This is the default build script taken from static-haskell-nix.
# We've adjusted some things and removed some instructions that
# we didn't have a usecase for.
#
# Run using:
#
#     $(nix-build --no-link -A full-build-script)
#
# The invocation above will build a buildscript and then run it.
# (The output of `nix-build --no-link -A full-build-script` is a
# path to the build script. The `$()` invokes the script.)
#
# That will:
#
#   - Fetch a stackage snapshot and run stack2nix on it. This
#     yields a bunch of nix derivations of the entire snapshot.
#     Some of these will be used in the actual build for the
#     vaultenv package.
#   - Download or build a GHC compiled with musl and integer-simple.
#   - Build vaultenv with integer-simple and link everything
#     statically. Dependencies are taken from stack.yaml in this
#     repository.
#
# The final output of the invocation above will print a Nix store path
# like `/nix/store/ak6qm8qmb66zwri7y16sqina1pvn0gmz-vaultenv-real-0.10.0`
# which will contain a fully static vaultenv binary in the `bin/` subdir.
{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
}:
let
  cabal-package-name = "vaultenv-real";

  # This has to match the compiler used in the Stackage snapshot.
  # Update this when the Stackage snapshot changes the version of
  # GHC it uses.
  compiler = "ghc865";

  # Pin static-haskell-nix version.
  static-haskell-nix = fetchTarball https://github.com/nh2/static-haskell-nix/archive/ff7715e0e13fb3f615e64a8d8c2e43faa4429b0f.tar.gz;

  # Pin the version of nixpkgs to the one from `static-haskell-nix`.
  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  # Generate a stack2nix script which will download a Stackage + Hackage
  # snapshot and convert it to Nix derivations for use in our final build
  # script.
  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.;
    # Also pin the Hackage snapshot to a certain time for extra-deps without
    # hashes or revisions. Vaultenv doesn't have any, but it's there if it
    # ever turns out we need it.
    hackageSnapshot = "2019-10-08T00:00:00Z";
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    integer-simple = true;
    cabalPackageName = cabal-package-name;
    inherit compiler stack2nix-output-path;
  };

  # This build script invokes `nix-build` on this same file using nix build
  # from a pinned version of nixpkgs. It will build the `static_package`
  # attribute that we define in the set below.
  full-build-script = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!/usr/bin/env bash
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static-package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

in
  {
    inherit full-build-script;
    static-package = static-stack2nix-builder.static_package;
  }
