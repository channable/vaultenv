# How to release `vaultenv`

 1. Increment the `version` field in `package.yaml`
 1. Increment the `version` in `src/Config.hs` in `optionsParserWithInfo`
 1. Create a git commit
 1. Tag: `git tag -a v<VERSION>`. Write a changelog.
 1. Compile and package. Take care to use libc from at least Xenial.
 1. `git push origin master --tags`
 1. Go to https://github.com/channable/vaultenv/releases
 1. Click "Draft a new release"
 1. Paste the changelog and attach the binaries
