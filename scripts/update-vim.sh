#!/bin/bash
set -euo pipefail

[[ ! -f ~/.vim/autoload/plug.vim ]] && {
	echo "Vim-Plug is missing. Installing ..."
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	echo "Vim-Plug installed"
}

echo "$(tput setaf 8)Update Vim plugins$(tput sgr 0)"
vim +PlugUpgrade +PlugUpdate +PlugClean! +qall

echo "$(tput setaf 8)Update CoC extensions$(tput sgr 0)"
vim +CocInstall +CocUpdateSync +qall

echo "Vim updated"
