#!/bin/bash
# shellcheck disable=SC2091,SC2103

# This script builds a .deb package from the static binary build by Nix.
#
#   Usage: ./scripts/build_package.sh

# Safe scripting options:
#  - `e` for fail on nonzero exit of any command
#  - `u` for fail on unset variables
#  - `f` to disable globbing
#  - `o pipefail` for error on failed pipes
set -euf -o pipefail

# Get the package version from Stack, eliminate the single quotes.
# Exported, because it is used with envsubst to write the control file.
VERSION=$(stack query locals vaultenv-real version | sed -e "s/^'//" -e "s/'$//")
export VERSION

# The name of the .deb file to create
PKGNAME="vaultenv-${VERSION}"

cd ..
VAULTENV_NIX_PATH=$($(nix-build --no-link -A full-build-script nix/vaultenv-static.nix) | tail -n1)
cd -

cp --no-preserve=mode,ownership "${VAULTENV_NIX_PATH}/bin/vaultenv" "vaultenv-${VERSION}-linux-musl"


# Build the Debian package
# We need to use `fakeroot` here to have the package files be correctly owned
# by `root` without being `root` ourselves.
fakeroot -- bash <<-EOFAKEROOT
  # Re-enable safe scripting options since this is a new shell
  set -euf -o pipefail

  # Ensure created directories are mode 0755 (default is 0775)
  umask 022

  # Recreate the file system layout as it should be on the target machine.
  mkdir -p "$PKGNAME/DEBIAN"
  mkdir -p "$PKGNAME/usr/bin"
  mkdir -p "$PKGNAME/etc/secrets.d"

  # Write the package metadata file, substituting environment variables in the
  # template file.
  cat deb_control | envsubst > "$PKGNAME/DEBIAN/control"

  cp deb_postinst "$PKGNAME/DEBIAN/postinst"
  chmod 0755 "$PKGNAME/DEBIAN/postinst"

  # Copy the built binary from the Nix store to the target directory
  cp --no-preserve=mode,ownership "${VAULTENV_NIX_PATH}/bin/vaultenv" "$PKGNAME/usr/bin/"
  chown root:root "$PKGNAME/usr/bin/vaultenv"
  chmod 0755 "$PKGNAME/usr/bin/vaultenv"
  dpkg-deb --build "$PKGNAME"
EOFAKEROOT

# Clean up temporary files
rm -fr "$PKGNAME"
