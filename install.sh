#!/usr/bin/env bash

# Vim
ln -sf ~/dotfiles/vimrc                 ~/.vimrc
ln -sf ~/dotfiles/vimrc.local           ~/.vimrc.local
ln -sf ~/dotfiles/vimrc.local.bundles   ~/.vimrc.local.bundles

# Ctags
ln -sf ~/dotfiles/ctags                 ~/.ctags

# i3
rm -rf ~/.config/i3
ln -sf ~/dotfiles/i3                    ~/.config

# Git
mkdir ~/.config/git
ln -sf ~/dotfiles/git/ignore            ~/.config/git/ignore

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean +qall

# Setup YouCompleteMe
python3 ~/.vim/plugged/youcompleteme/install.py
