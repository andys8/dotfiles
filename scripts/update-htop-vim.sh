#!/bin/bash
set -euo pipefail

TMPDIR=/tmp/htop-vim-installation
COMMIT=70c91d6d598f15b0311ae6779810e036133e72d2
TARBALL=https://github.com/KoffeinFlummi/htop-vim/tarball/$COMMIT
EXPECTED_VERSION="htop 2.1.0 - (C) 2004-2020 Hisham Muhammad"
mkdir -p $TMPDIR
cd $TMPDIR

# check if install can be skipped
VERSION=$(htop --version | head -1)
[[ $VERSION == "$EXPECTED_VERSION" ]] && {
	echo "htop-vim version '$VERSION' is already installed."
	exit 0
}

# Download tarball
curl -L $TARBALL | tar xz

# make binary
cd $TMPDIR/KoffeinFlummi-htop-vim-*
./autogen.sh
./configure
make

echo "Please enter password for 'make install' (htop needs to be available for root user)"
sudo make install

# check if successful
VERSION=$(htop --version | head -1)
[[ $VERSION != "$EXPECTED_VERSION" ]] && {
	echo "htop-vim version '$VERSION' is not expected. Install failed."
	exit 1
}
