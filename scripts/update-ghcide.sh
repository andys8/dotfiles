#!/bin/sh
set -eu

LINUX=$(lsb_release -i -s)

if [ "$LINUX" = "LinuxMint" ]; then
	nix-env -iA ghcide-ghc865 -f https://github.com/cachix/ghcide-nix/tarball/master
	echo ">> ghcide installed with nix"
	exit 0
elif [ "$LINUX" = "ManjaroLinux" ]; then
	! command -v "yay" >/dev/null 2>&1 && {
		echo "yay missing"
		exit 1
	}
	yay --editmenu -S ghcide
	exit 0
else
	echo "Unexpected distribution: $LINUX"
	exit 1
fi

