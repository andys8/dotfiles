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
mkdir -p ~/.config/fish/functions
ln -sf ~/dotfiles/fish/config.fish ~/.config/fish/config.fish
ln -sf ~/dotfiles/fish/killport.fish ~/.config/fish/functions/killport.fish
ln -sf ~/dotfiles/fish/hostip.fish ~/.config/fish/functions/hostip.fish
rm -rf ~/.config/omf
ln -sf ~/dotfiles/omf ~/.config

# Bash
ln -sf ~/dotfiles/bashrc ~/.bashrc

# Xmonad
mkdir -p ~/.xmonad
ln -sf ~/dotfiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -sf ~/dotfiles/xmonad/xmobar.config ~/.xmonad/xmobar.config
ln -sf ~/dotfiles/xmonad/startup.sh ~/.xmonad/startup.sh
ln -sf ~/dotfiles/xmonad/stack.yaml ~/.xmonad/stack.yaml
ln -sf ~/dotfiles/xmonad/package.yaml ~/.xmonad/package.yaml

# Ranger
mkdir -p ~/.config/ranger
ln -sf ~/dotfiles/rc.conf ~/.config/ranger/rc.conf

# Qutebrowser
mkdir -p ~/.config/qutebrowser
ln -sf ~/dotfiles/qutebrowser/config.py ~/.config/qutebrowser/config.py
ln -sf ~/dotfiles/qutebrowser/dracula.py ~/.config/qutebrowser/dracula.py

# bin
mkdir -p ~/bin
ln -sf ~/dotfiles/bin/emoji ~/bin/emoji
ln -sf ~/dotfiles/bin/hoogle-rofi ~/bin/hoogle-rofi
ln -sf ~/dotfiles/bin/image-info ~/bin/image-info
ln -sf ~/dotfiles/bin/lastpass-rofi ~/bin/lastpass-rofi
ln -sf ~/dotfiles/bin/lock ~/bin/lock
ln -sf ~/dotfiles/bin/script-rofi ~/bin/script-rofi

# sxiv
mkdir -p ~/.config/sxiv/exec
ln -sf ~/dotfiles/bin/image-info ~/.config/sxiv/exec/image-info

# pulseaudio
mkdir -p ~/.config/pulse
ln -sf ~/dotfiles/pulseaudio/client.conf ~/.config/pulse/client.conf

# ghci
ln -sf ~/dotfiles/ghci ~/.ghci

# Xresources
ln -sf ~/dotfiles/Xresources ~/.Xresources

# stalonetray
ln -sf ~/dotfiles/stalonetrayrc ~/.stalonetrayrc

echo "Links updated"
