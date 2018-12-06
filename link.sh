#!/usr/bin/env bash

# Vim
ln -sf ~/dotfiles/vimrc                     ~/.vimrc
ln -sf ~/dotfiles/vimrc.local               ~/.vimrc.local
ln -sf ~/dotfiles/vimrc.local.bundles       ~/.vimrc.local.bundles

# Ctags
ln -sf ~/dotfiles/ctags                     ~/.ctags

# i3
rm -rf ~/.config/i3
ln -sf ~/dotfiles/i3                        ~/.config

# rofi (dmenu for i3)
rm -rf ~/.config/rofi
ln -sf ~/dotfiles/rofi                      ~/.config

# Git
mkdir -p ~/.config/git
ln -sf ~/dotfiles/git/ignore                ~/.config/git/ignore

# Alacritty
rm -rf ~/.config/alacritty
mkdir -p ~/.config/alacritty
ln -sf ~/dotfiles/alacritty/alacritty.yml   ~/.config/alacritty/alacritty.yml

