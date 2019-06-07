# How to release `vaultenv`

 1. Increment the `version` field in `package.yaml`
 1. Create a git commit
 1. Tag: `git tag -a v<VERSION>`. Write a changelog.
 1. `git push origin master --tags`
 1. Travis now builds a new release.
 1. Go to https://github.com/channable/vaultenv/releases
 1. Click "Draft a new release"
 1. Paste the changelog. The binaries should be there already.
