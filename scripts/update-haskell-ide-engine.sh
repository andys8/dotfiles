#!/bin/sh
set -eu

command -v "hie-wrapper" >/dev/null 2>&1 && {
	VERSION=$(hie-wrapper --version)
	echo "haskell-ide-engine is already installed."
	echo "$VERSION"
	exit 0
}

LINUX=$(lsb_release -i -s)

if [ "$LINUX" = "LinuxMint" ]; then

	HASH="c6e93d2d641ef48703eabed8ec5cde3d774cb0e5" # 0.14.0.0
	cachix use all-hies
	nix-env -iA selection --arg selector 'p: { inherit (p) ghc865; }' -f https://github.com/infinisil/all-hies/tarball/$HASH
	echo ">> haskell-ide-engine installed with nix"
	exit 0

elif [ "$LINUX" = "ManjaroLinux" ]; then

	! command -v "yay" >/dev/null 2>&1 && {
		echo "yay missing"
		exit 1
	}
	yay --editmenu -S haskell-ide-engine
	exit 0

else
	echo "Unexpected distribution: $LINUX"
	exit 1
fi
