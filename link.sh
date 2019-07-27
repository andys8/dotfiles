#!/bin/bash
set -euo pipefail

# Vim
ln -sf ~/dotfiles/vimrc                     ~/.vimrc
ln -sf ~/dotfiles/vimrc.local               ~/.vimrc.local
ln -sf ~/dotfiles/vimrc.local.bundles       ~/.vimrc.local.bundles
ln -sf ~/dotfiles/coc-settings.json         ~/.vim/coc-settings.json
 
# Idea VIM
ln -sf ~/dotfiles/ideavimrc                 ~/.ideavimrc

# Ctags
ln -sf ~/dotfiles/ctags                     ~/.ctags

# i3
rm -rf ~/.config/i3
ln -sf ~/dotfiles/i3                        ~/.config

# rofi (dmenu for i3)
rm -rf ~/.config/rofi
ln -sf ~/dotfiles/rofi                      ~/.config

# Git
rm -rf ~/.config/git
ln -sf ~/dotfiles/git                       ~/.config
ln -sf ~/dotfiles/gitconfig                 ~/.gitconfig
ln -sf ~/dotfiles/tigrc                     ~/.tigrc

# Alacritty
ln -sf ~/dotfiles/alacritty.yml             ~/.alacritty.yml

# Profile
ln -sf ~/dotfiles/profile                   ~/.profile

# Fish
ln -sf ~/dotfiles/config.fish               ~/.config/fish/config.fish
rm -rf ~/.config/omf
ln -sf ~/dotfiles/omf                       ~/.config

# Bash
ln -sf ~/dotfiles/bashrc                    ~/.bashrc

# Google Java Format
ln -sf ~/dotfiles/google-java-format        ~/bin/google-java-format

