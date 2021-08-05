#!/bin/sh

# Vendored Nix installation script.
# Retrieved 2020-03-11 from https://nixos.org/nix/install

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
    Linux.x86_64) system=x86_64-linux; hash=bb1bc35b55b1b199e391000ad12bb1b9d39114765a6a2f6110e458a046624ed6;;
    Linux.i?86) system=i686-linux; hash=8ca315121cf8ae23306f46f816fc3e3c7daec7b21009f8673d9c77f690a51071;;
    Linux.aarch64) system=aarch64-linux; hash=d9fd7cbc1bf64e3311c4309f2d5cde87039c348927df299f8c52d930eeef6eb5;;
    Darwin.x86_64) system=x86_64-darwin; hash=532e3ad482ba18b556a25041a101ee96fe7ae5718679728ccfbbea34bb169018;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

url="https://nixos.org/releases/nix/nix-2.3.3/nix-2.3.3-$system.tar.xz"

tarball="$tmpDir/$(basename "$tmpDir/nix-2.3.3-$system.tar.xz")"

require_util curl "download the binary tarball"
require_util tar "unpack the binary tarball"

echo "downloading Nix 2.3.3 binary tarball for $system from '$url' to '$tmpDir'..."
curl -L "$url" -o "$tarball" --retry 3 || oops "failed to download '$url'"

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
