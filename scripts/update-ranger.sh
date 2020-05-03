#!/bin/bash
set -euo pipefail

PLUGIN_DIR=~/.config/ranger/plugins

function install_plugin() {
	REPO="$1"
	DIR="$2"

	echo "Ranger plugin $REPO"

	if [[ ! -d $DIR ]]; then
		git clone "$REPO" "$DIR"
	else
		(cd "$DIR" && git pull)
	fi
}

mkdir -p $PLUGIN_DIR

# devicons
install_plugin https://github.com/andys8/ranger_devicons $PLUGIN_DIR/ranger-devicons

# autojump
install_plugin https://github.com/fdw/ranger-autojump $PLUGIN_DIR/ranger-autojump
ln -sf $PLUGIN_DIR/ranger-autojump/autojump.py $PLUGIN_DIR/autojump.py
