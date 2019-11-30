#!/bin/bash
set -euo pipefail

# Vim
mkdir -p ~/.vim
ln -sf ~/dotfiles/vimrc ~/.vimrc
ln -sf ~/dotfiles/vimrc.local ~/.vimrc.local
ln -sf ~/dotfiles/vimrc.local.bundles ~/.vimrc.local.bundles
ln -sf ~/dotfiles/coc-settings.json ~/.vim/coc-settings.json

# Idea VIM
ln -sf ~/dotfiles/ideavimrc ~/.ideavimrc

# Ctags
ln -sf ~/dotfiles/ctags ~/.ctags

# i3
rm -rf ~/.config/i3
ln -sf ~/dotfiles/i3 ~/.config

# rofi (dmenu for i3)
rm -rf ~/.config/rofi
ln -sf ~/dotfiles/rofi ~/.config

# Git
rm -rf ~/.config/git
ln -sf ~/dotfiles/git ~/.config
ln -sf ~/dotfiles/gitconfig ~/.gitconfig
ln -sf ~/dotfiles/tigrc ~/.tigrc

# Alacritty
ln -sf ~/dotfiles/alacritty.yml ~/.alacritty.yml

# Profile
ln -sf ~/dotfiles/profile ~/.profile

# Fish
ln -sf ~/dotfiles/fish/config.fish ~/.config/fish/config.fish
ln -sf ~/dotfiles/fish/killport.fish ~/.config/fish/functions/killport.fish
rm -rf ~/.config/omf
ln -sf ~/dotfiles/omf ~/.config

# Bash
ln -sf ~/dotfiles/bashrc ~/.bashrc

# Google Java Format
ln -sf ~/dotfiles/google-java-format ~/bin/google-java-format

# Xmonad
ln -sf ~/dotfiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -sf ~/dotfiles/xmonad/xmobar.config ~/.xmonad/xmobar.config
ln -sf ~/dotfiles/xmonad/startup.sh ~/.xmonad/startup.sh
ln -sf ~/dotfiles/xmonad/stack.yaml ~/.xmonad/stack.yaml
ln -sf ~/dotfiles/xmonad/package.yaml ~/.xmonad/package.yaml

# Ranger
ln -sf ~/dotfiles/rc.conf ~/.config/ranger/rc.conf

# Qutebrowser
ln -sf ~/dotfiles/qutebrowser/config.py ~/.config/qutebrowser/config.py
ln -sf ~/dotfiles/qutebrowser/dracula.py ~/.config/qutebrowser/dracula.py
