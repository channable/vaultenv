# How to release `vaultenv`

 1. Build `vaultenv` with `stack`. Run the tests.
 1. Increment the `version` field in `package.yaml`
 1. Create a git commit
 1. Tag: `git tag -a v<VERSION>`. Write a changelog.
 1. `git push origin master --tags`
 1. Build a static version of `vaultenv` by following the instructions in
    `default.nix`.
 1. Go to https://github.com/channable/vaultenv/releases
 1. Click "Draft a new release". Add a binary from the Nix output.
