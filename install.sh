#!/usr/bin/env bash

# Link
ln -sf ~/dotfiles/vimrc                ~/.vimrc
ln -sf ~/dotfiles/vimrc.local          ~/.vimrc.local
ln -sf ~/dotfiles/vimrc.local.bundles  ~/.vimrc.local.bundles


# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean +qall

# Setup YouCompleteMe
python3 ~/.vim/plugged/youcompleteme/install.py
