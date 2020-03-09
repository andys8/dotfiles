#!/bin/sh
set -eu

command -v "ghcide" >/dev/null 2>&1 && {
	VERSION=$(ghcide --version)
	echo "ghcide is already installed."
	echo "$VERSION"
	exit 0
}

LINUX=$(lsb_release -i -s)

if [ "$LINUX" = "LinuxMint" ]; then

	# install with stack
	cd /tmp
	git clone https://github.com/digital-asset/ghcide.git
	cd ghcide
	stack install
	echo ">> ghcide installed with stack"
	exit 0

elif [ "$LINUX" = "ManjaroLinux" ]; then

	! command -v "yay" >/dev/null 2>&1 && {
		echo "yay missing"
		exit 1
	}
	yay -S ghcide --noconfirm
	exit 0

else
	echo "Unexpected distribution: $LINUX"
	exit 1
fi
