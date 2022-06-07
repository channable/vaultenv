# This is the default build script taken from static-haskell-nix.
# We've adjusted some things and removed some instructions that
# we didn't have a usecase for.
#
# Run using:
#
#     $(nix-build --no-link -A full-build-script nix/vaultenv-static.nix)
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
  # Field from the hpack/cabal file. We append `-real` to work around
  # vaultenv also being included in `nixpgks`, which leads to conflicts.
  cabal-package-name = "vaultenv-real";

  # This has to match the compiler used in the Stackage snapshot.
  # Update this when the Stackage snapshot changes the version of
  # GHC it uses.
  compiler = "ghc902";

  # Pin versions of static-haskell-nix and nixpkgs.
  sources = import ./sources.nix;
  static-haskell-nix = sources.static-haskell-nix;
  pkgs = import ./nixpkgs-pinned.nix {};

  # Generate a stack2nix script which will download a Stackage + Hackage
  # snapshot and convert it to Nix derivations for use in our final build
  # script.
  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs compiler;
    stack-project-dir = toString ../.;
    # Also pin the Hackage snapshot to a certain time for extra-deps without
    # hashes or revisions. Vaultenv doesn't have any, but it's there if it
    # ever turns out we need it.
    hackageSnapshot = "2019-10-08T00:00:00Z";
    stack-yaml = "stack-static-build.yaml";
  };

  # `full-build-script` will eventually build the `static_package` attribute of
  # this set. Note `nix-build -A static-package` in `full-build-script` and
  # `static-package` in the `in` of this module.
  #
  # The `static_package` attribute boils down to `haskellPackages.vaultenv-real`
  # with modifications to get static builds to work. `haskellPackages` is
  # Nixpkgs' infrastructure for Haskell applications and libraries.
  #
  # Take a look at `survey/default.nix` in the `static-haskell-nix` repository
  # if you want to know what patches have been applied to `haskellPackages`.
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
    ${pkgs.nix}/bin/nix-build --no-link -A static-package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" nix/vaultenv-static.nix "$@"
  '';

in
  {
    inherit full-build-script;
    static-package = static-stack2nix-builder.static_package;
  }
