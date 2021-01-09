#!/bin/bash
set -euo pipefail

command -v "htop" >/dev/null 2>&1 && {
    VERSION=$(htop --version | head -1)
    echo "htop ($VERSION) is already installed."
    exit 0
}


REV=3.0.3vim
TARBALL=https://github.com/KoffeinFlummi/htop-vim/tarball/$REV
TMP_DIR=/tmp/htop-vim-installation-$REV

mkdir -p $TMP_DIR
cd $TMP_DIR

# Download tarball
curl -L $TARBALL | tar xz

# make binary
cd $TMP_DIR/KoffeinFlummi-htop-vim-*

./autogen.sh
./configure
make

echo "Please enter password for 'make install' (htop needs to be available for root user)"
sudo make install
