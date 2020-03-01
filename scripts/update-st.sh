#!/bin/bash
set -euo pipefail

GIT=https://github.com/andys8/st
TMP_DIR=/tmp/st-installation

command -v "st" >/dev/null 2>&1 && {
	echo "st is already installed"
	exit 0
}

mkdir -p $TMP_DIR
git clone $GIT $TMP_DIR
cd $TMP_DIR

echo "Please enter password for 'make install' (st needs to be available for root user)"
sudo make install
tic -sx st.info
