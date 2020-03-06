#!/bin/sh
set -eu

LINUX=$(lsb_release -i -s)

if [ "$LINUX" = "LinuxMint" ]; then

	command -v "ghcide" >/dev/null 2>&1 && {
		echo "ghcide is already installed"
		exit 0
	}

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

