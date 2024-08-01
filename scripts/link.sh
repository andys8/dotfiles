#!/bin/bash
set -euo pipefail

# .config dir (if missing)
mkdir -p ~/.config

# Vim
mkdir -p ~/.vim
ln -sf ~/dotfiles/vimrc ~/.vimrc
ln -sf ~/dotfiles/vimrc.local ~/.vimrc.local
ln -sf ~/dotfiles/vimrc.local.bundles ~/.vimrc.local.bundles
ln -sf ~/dotfiles/coc-settings.json ~/.vim/coc-settings.json
rm -rf ~/.vim/snippets
ln -sf ~/dotfiles/vim-snippets ~/.vim/snippets

# Idea VIM
ln -sf ~/dotfiles/ideavimrc ~/.ideavimrc

# rofi
rm -rf ~/.config/rofi
ln -sf ~/dotfiles/rofi ~/.config

# Git
rm -rf ~/.config/git
ln -sf ~/dotfiles/git ~/.config
ln -sf ~/dotfiles/gitconfig ~/.gitconfig
ln -sf ~/dotfiles/tigrc ~/.tigrc

# Ack
ln -sf ~/dotfiles/ackrc ~/.ackrc

# Ripgrep
ln -sf ~/dotfiles/ripgreprc ~/.ripgreprc

# Alacritty
ln -sf ~/dotfiles/alacritty.toml ~/.alacritty.toml

# Profile
ln -sf ~/dotfiles/profile ~/.profile

# Fish
mkdir -p ~/.config/fish/functions
ln -sf ~/dotfiles/fish/config.fish ~/.config/fish/config.fish
ln -sf ~/dotfiles/fish/fkill.fish ~/.config/fish/functions/fkill.fish
ln -sf ~/dotfiles/fish/hostip.fish ~/.config/fish/functions/hostip.fish
ln -sf ~/dotfiles/fish/killport.fish ~/.config/fish/functions/killport.fish
ln -sf ~/dotfiles/fish/sudo.fish ~/.config/fish/functions/sudo.fish
ln -sf ~/dotfiles/fish/mkcd.fish ~/.config/fish/functions/mkcd.fish
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
ln -sf ~/dotfiles/xmonad/fourmolu.yaml ~/.xmonad/fourmolu.yaml

# Ranger
mkdir -p ~/.config/ranger
ln -sf ~/dotfiles/rc.conf ~/.config/ranger/rc.conf

# Qutebrowser
mkdir -p ~/.config/qutebrowser
ln -sf ~/dotfiles/qutebrowser/config.py ~/.config/qutebrowser/config.py
ln -sf ~/dotfiles/qutebrowser/dracula.py ~/.config/qutebrowser/dracula.py

# sxiv
mkdir -p ~/.config/sxiv/exec
ln -sf ~/dotfiles/sxiv/key-handler ~/.config/sxiv/exec/key-handler
ln -sf ~/dotfiles/sxiv/image-info ~/.config/sxiv/exec/image-info
ln -sf ~/dotfiles/sxiv/image-info ~/bin/image-info

# pulseaudio
mkdir -p ~/.config/pulse
ln -sf ~/dotfiles/pulseaudio/client.conf ~/.config/pulse/client.conf

# ghci
ln -sf ~/dotfiles/ghci ~/.ghci

# Xresources
ln -sf ~/dotfiles/Xresources ~/.Xresources

# markdownlint
ln -sf ~/dotfiles/markdownlintrc ~/.markdownlintrc

# bottom
mkdir -p ~/.config/bottom
ln -sf ~/dotfiles/bottom.toml ~/.config/bottom/bottom.toml

# Dunst (notifications)
mkdir -p ~/.config/dunst
ln -sf ~/dotfiles/dunstrc ~/.config/dunst/dunstrc

# Ignore in ripgrep and others
ln -sf ~/dotfiles/ignore ~/.ignore

# Screenshots
mkdir -p ~/Pictures/screenshots

# autorandr
mkdir -p ~/.config/autorandr
ln -sf ~/dotfiles/autorandr/postswitch ~/.config/autorandr/postswitch

# nix
mkdir -p ~/.nixpkgs
ln -sf ~/dotfiles/nix/config.nix ~/.nixpkgs/config.nix

# zathura
mkdir -p ~/.config/zathura
ln -sf ~/dotfiles/zathurarc ~/.config/zathura/zathurarc

# lsd
mkdir -p ~/.config/lsd
ln -sf ~/dotfiles/lsd.yaml ~/.config/lsd/config.yaml

# rofi
rm -rf ~/.config/udiskie
ln -sf ~/dotfiles/udiskie ~/.config

# Purty
# Use purty binary directly, avoid node spawning a process (performance)
ln -sf ~/.npm-global/lib/node_modules/purty/bin/linux/purty ~/bin/purty

# ghcup
mkdir -p ~/.ghcup
ln -sf ~/dotfiles/ghcup/config.yaml ~/.ghcup/config.yaml

# Clean up symlinks
~/dotfiles/scripts/clean-symlinks.sh

echo "Links updated"
