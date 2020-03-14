#!/bin/sh
set -eu

LINUX=$(lsb_release -i -s)
if [ "$LINUX" = "LinuxMint" ]; then
	sudo apt-get update
	sudo apt-get upgrade
elif [ "$LINUX" = "ManjaroLinux" ]; then
	yay -Syu
else
	echo "Unexpected distribution: $LINUX"
	exit 1
fi
