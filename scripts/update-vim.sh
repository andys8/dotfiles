#!/bin/bash
set -euo pipefail

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo "Update Vim plugins"
vim +PlugInstall +qall!

echo "Update CoC extensions"
vim +CocInstall +qall!
vim +CocUpdateSync +qall!

echo "Vim updated"
