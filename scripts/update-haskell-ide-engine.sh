#!/bin/sh
set -eu

LINUX=$(lsb_release -i -s)

if [ "$LINUX" = "LinuxMint" ]; then

	HASH="c6e93d2d641ef48703eabed8ec5cde3d774cb0e5" # 0.14.0.0
	cachix use all-hies
	nix-env -iA selection --arg selector 'p: { inherit (p) ghc865; }' -f https://github.com/infinisil/all-hies/tarball/$HASH
	echo ">> haskell-ide-engine installed with nix"
	exit 0

elif [ "$LINUX" = "ManjaroLinux" ]; then

	VERSION="1.1-4"
	OUTPUTDIR="$HOME/bin"
	VERSIONFILE="$OUTPUTDIR/hie-version.txt"

	touch "$VERSIONFILE"
	PREV_VERSION=$(cat "$VERSIONFILE")

	if [ $VERSION = "$PREV_VERSION" ]; then
		echo ">> haskell-ide-engine $VERSION already installed"
		exit 0
	fi

	cd /tmp
	curl https://repo.archlinuxcn.org/x86_64/haskell-ide-engine-$VERSION-x86_64.pkg.tar.zst -o hie-$VERSION.tar.zst
	tar -I zstd -xvf hie-$VERSION.tar.zst
	cp usr/bin/* "$OUTPUTDIR/"
	echo $VERSION >"$VERSIONFILE"
	echo ">> haskell-ide-engine $VERSION binary downloaded"
	exit 0

else
	echo "Unexpected distribution: $LINUX"
	exit 1
fi

