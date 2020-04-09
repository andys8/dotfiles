#!/bin/bash
set -eu

LINUX=$(lsb_release -i -s)
CLEANUP=$((RANDOM % 20))

if [ "$LINUX" = "LinuxMint" ]; then
	echo "Update system"
	sudo apt update
	sudo apt upgrade
	if [ $CLEANUP -eq 0 ]; then
		echo "Cleanup"
		sudo apt clean
		sudo apt autoremove
	fi
elif [ "$LINUX" = "ManjaroLinux" ]; then
	echo "Update system"
	yay -Syu
	if [ $CLEANUP -eq 0 ]; then
		echo "Cleanup"
		yay --clean
		yay -Sc --noconfirm
	fi
else
	echo "Unexpected distribution: $LINUX"
	exit 1
fi
