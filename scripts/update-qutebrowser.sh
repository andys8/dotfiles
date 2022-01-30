#!/bin/bash
# Update qutebrowser on different operating systems

set -euo pipefail

VERSION_MINT="2.4.0"
VERSION_MINT_ALL="$VERSION_MINT-1"

LINUX=$(lsb_release -i -s)
LINUX=${LINUX,,}

command -v "qutebrowser" >/dev/null 2>&1 && {
    VERSION=$(qutebrowser --version | grep "qutebrowser v")
    echo "qutebrowser ($VERSION) is already installed."

    # Check if newer on mint
    if [ "$LINUX" = "linuxmint" ] && [[ ! "$VERSION" == *"$VERSION_MINT"* ]]; then
        echo "Will install version $VERSION_MINT_ALL"
    else
        exit 0
    fi
}

if [ "$LINUX" = "linuxmint" ]; then

    BASE_URL=http://ftp.de.debian.org/debian/pool/main/q/qutebrowser
    DEB1=qutebrowser_${VERSION_MINT_ALL}_all.deb
    DEB2=qutebrowser-qtwebengine_${VERSION_MINT_ALL}_all.deb

    echo "Download qutebrowser packages"
    curl "$BASE_URL/$DEB1" -o "/tmp/$DEB1" -v
    curl "$BASE_URL/$DEB2" -o "/tmp/$DEB2" -v

    echo "Qutebrowser: dpkg -i (please enter password)"
    sudo apt install "/tmp/$DEB1" "/tmp/$DEB2"
    echo "Qutebrowser: Fix dependencies "
    sudo apt-get install --fix-broken --assume-yes

elif [ "$LINUX" = "manjarolinux" ]; then

    ! command -v "yay" >/dev/null 2>&1 && {
        echo "yay missing"
        exit 1
    }
    yay -S qutebrowser --noconfirm

else
    echo "Unexpected distribution: $LINUX"
    exit 1
fi
