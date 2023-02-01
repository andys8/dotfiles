#!/bin/bash
set -euo pipefail

echo "Checking vim-plug"
[[ ! -f ~/.vim/autoload/plug.vim ]] && {
    echo "Vim-Plug is missing. Installing ..."
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    echo "Vim-Plug installed"
}

echo "Update Vim plugins"
vim +PlugUpgrade +PlugUpdate +PlugClean! +qall

echo "Update CoC extensions"
vim +CocInstall +qall
vim +CocUpdateSync +qall

echo "Vim updated"
