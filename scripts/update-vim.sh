#!/bin/bash
set -euo pipefail

[[ ! -f ~/.vim/pack/jetpack/opt/vim-jetpack/plugin/jetpack.vim ]] && {
    echo "vim-jetpack is missing. Installing ..."
    curl -fLo ~/.vim/pack/jetpack/opt/vim-jetpack/plugin/jetpack.vim \
        --create-dirs https://raw.githubusercontent.com/tani/vim-jetpack/master/plugin/jetpack.vim
    echo "vim-jetpack installed"
}

echo "Update Vim plugins"
vim +JetpackSync +qall!

echo "Update CoC extensions"
vim +CocInstall +qall!
vim +CocUpdateSync +qall!

echo "Vim updated"
