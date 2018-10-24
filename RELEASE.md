# How to release `vaultenv`

 1. Increment the `version` field in `package.yaml`
 2. Increment the `version` in `src/Config.hs` in `optionsParserWithInfo`
 3. Create a git commit
 4. Tag: `git tag -a v<VERSION>`. Write a changelog.
 5. Compile and package. Take care to use libc from at least Xenial.
 6. `git push origin master --tags`
 7. Go to https://github.com/channable/vaultenv/releases
 8. Click "Draft a new release"
 9. Paste the changelog and attach the binaries
