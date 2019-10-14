#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded
oops() {
    echo "$0:" "$@" >&2
    exit 1
}

tmpDir="$(mktemp -d -t nix-binary-tarball-unpack.XXXXXXXXXX || \
          oops "Can't create temporary directory for downloading the Nix binary tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

require_util() {
    command -v "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

case "$(uname -s).$(uname -m)" in
    Linux.x86_64) system=x86_64-linux; hash=107310e95a57f502b7ada41aa2c76a3d4aec18847ad864d0b233ca8f4452e9fc;;
    Linux.i?86) system=i686-linux; hash=a5d3f26d4a449616bf654286f2fe29c1c1df4f029b7e29fa3ccf8494d598bfee;;
    Linux.aarch64) system=aarch64-linux; hash=94a6a525bd0b2df82e14b96b5b0eaae86669b5d4671aacfc4db2db85325a81c1;;
    Darwin.x86_64) system=x86_64-darwin; hash=a1f0108cedc74293fe23186b9e9d674e7a6779fc90005cb462649e52b28581cd;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

url="https://nixos.org/releases/nix/nix-2.3.1/nix-2.3.1-$system.tar.xz"

tarball="$tmpDir/$(basename "$tmpDir/nix-2.3.1-$system.tar.xz")"

require_util curl "download the binary tarball"
require_util tar "unpack the binary tarball"

echo "downloading Nix 2.3.1 binary tarball for $system from '$url' to '$tmpDir'..."
curl -L "$url" -o "$tarball" || oops "failed to download '$url'"

if command -v sha256sum > /dev/null 2>&1; then
    hash2="$(sha256sum -b "$tarball" | cut -c1-64)"
elif command -v shasum > /dev/null 2>&1; then
    hash2="$(shasum -a 256 -b "$tarball" | cut -c1-64)"
elif command -v openssl > /dev/null 2>&1; then
    hash2="$(openssl dgst -r -sha256 "$tarball" | cut -c1-64)"
else
    oops "cannot verify the SHA-256 hash of '$url'; you need one of 'shasum', 'sha256sum', or 'openssl'"
fi

if [ "$hash" != "$hash2" ]; then
    oops "SHA-256 hash mismatch in '$url'; expected $hash, got $hash2"
fi

unpack=$tmpDir/unpack
mkdir -p "$unpack"
tar -xf "$tarball" -C "$unpack" || oops "failed to unpack '$url'"

script=$(echo "$unpack"/*/install)

[ -e "$script" ] || oops "installation script is missing from the binary tarball!"
"$script" "$@"

} # End of wrapping
