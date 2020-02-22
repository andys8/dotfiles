#!/bin/bash
set -euo pipefail

TMPDIR=/tmp/htop-vim-installation
COMMIT=70c91d6d598f15b0311ae6779810e036133e72d2
TARBALL=https://github.com/KoffeinFlummi/htop-vim/tarball/$COMMIT
EXPECTED_VERSION="htop 2.1.0 - (C) 2004-2020 Hisham Muhammad"
mkdir -p $TMPDIR
cd $TMPDIR

# check if already installed
command -v "htop" >/dev/null 2>&1 && {

	# check if install can be skipped because version matches
	VERSION=$(htop --version | head -1)
	if [[ $VERSION == "$EXPECTED_VERSION" ]]; then
		echo "htop version ($VERSION) is already installed."
		exit 0
	else
		echo "htop version ($VERSION) is not expected."
	fi
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
	echo "htop version ($VERSION) is not expected after installation. Something went wrong."
	exit 1
}
