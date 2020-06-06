#!/bin/sh
set -eu

command -v "ghcide" >/dev/null 2>&1 && {
	VERSION=$(ghcide --version)
	echo "ghcide is already installed."
	echo "$VERSION"
	exit 0
}

REPO=andys8/ghcide
BRANCH=update-to-ghc-883
FOLDER=ghcide-$(shuf -i0-10000000 -n1)

# install with stack
cd /tmp
git clone -b $BRANCH --single-branch https://github.com/$REPO "$FOLDER"
cd "$FOLDER"
stack install
echo ">> ghcide installed with stack"
ghcide --version
exit 0
