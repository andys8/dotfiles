#!/bin/bash
set -euo pipefail

command -v "qutebrowser" >/dev/null 2>&1 && {
    VERSION=$(qutebrowser --version | grep "qutebrowser v")
    echo "qutebrowser ($VERSION) is already installed."
    exit 0
}

LINUX=$(lsb_release -i -s)
LINUX=${LINUX,,}

if [ "$LINUX" = "linuxmint" ]; then

    VERSION="1.13.1-1"
    BASE_URL=http://ftp.de.debian.org/debian/pool/main/q/qutebrowser
    DEB1=qutebrowser_${VERSION}_all.deb
    DEB2=qutebrowser-qtwebengine_${VERSION}_all.deb

    echo "Download qutebrowser packages"
    curl "$BASE_URL/$DEB1" -o "/tmp/$DEB1"
    curl "$BASE_URL/$DEB2" -o "/tmp/$DEB2"

    echo "Qutebrowser: dpkg -i (please enter password)"
    sudo dpkg -i --force-depends --force-depends-version "/tmp/$DEB1" "/tmp/$DEB2"
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
