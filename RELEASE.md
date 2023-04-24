# How to release `vaultenv`

 1. Build `vaultenv` with `stack`. Run the tests.
 1. Increment the `version` field in `package.yaml`
 1. Create a git commit
 1. Tag: `git tag -a v<VERSION>`. Write a changelog.
 1. `git push origin master --tags`
 1. Build the static version of `vaultenv` and the Debian package by running
    `nix shell --file default.nix -c ./package/build_package.sh`.
 1. Go to https://github.com/channable/vaultenv/releases
 1. Click "Draft a new release". Add the binary from the Nix output and the
    .deb package.
