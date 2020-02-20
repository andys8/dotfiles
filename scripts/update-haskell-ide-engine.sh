#!/bin/sh
set -eu

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
wget https://repo.archlinuxcn.org/x86_64/haskell-ide-engine-$VERSION-x86_64.pkg.tar.zst
tar -I zstd -xvf haskell-ide-engine-$VERSION-x86_64.pkg.tar.zst
cp usr/bin/* "$OUTPUTDIR/"
echo $VERSION >"$VERSIONFILE"

echo ""
echo ">> haskell-ide-engine $VERSION installed"
