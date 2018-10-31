# How to release `vaultenv`

 1. Increment the `version` field in `package.yaml`
 2. Create a git commit
 3. Tag: `git tag -a v<VERSION>`. Write a changelog.
 4. Compile and package. Take care to use libc from at least Xenial.
 5. `git push origin master --tags`
 6. Go to https://github.com/channable/vaultenv/releases
 7. Click "Draft a new release"
 8. Paste the changelog and attach the binaries
