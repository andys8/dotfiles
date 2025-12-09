#!/bin/bash
set -euo pipefail

# .config dir (if missing)
mkdir -p ~/.config

# bin dir (if missing)
mkdir -p ~/bin

# Vim
mkdir -p ~/.vim
ln -sf ~/dotfiles/vimrc ~/.vimrc
ln -sf ~/dotfiles/vimrc.local ~/.vimrc.local
ln -sf ~/dotfiles/vimrc.local.bundles ~/.vimrc.local.bundles
ln -sf ~/dotfiles/coc-settings.json ~/.vim/coc-settings.json
rm -rf ~/.vim/snippets
ln -sf ~/dotfiles/vim-snippets ~/.vim/snippets

# Neovim
rm -rf ~/.config/nvim
ln -sf ~/dotfiles/nvim ~/.config/nvim

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
ln -sf ~/dotfiles/fish/fcd.fish ~/.config/fish/functions/fcd.fish
ln -sf ~/dotfiles/fish/worktree.fish ~/.config/fish/functions/worktree.fish
rm -rf ~/.config/omf
ln -sf ~/dotfiles/omf ~/.config

# Bash
ln -sf ~/dotfiles/bashrc ~/.bashrc

# Ranger
mkdir -p ~/.config/ranger
ln -sf ~/dotfiles/rc.conf ~/.config/ranger/rc.conf

# markdownlint
ln -sf ~/dotfiles/markdownlintrc ~/.markdownlintrc

# bottom
mkdir -p ~/.config/bottom
ln -sf ~/dotfiles/bottom.toml ~/.config/bottom/bottom.toml

# Ignore in ripgrep and others
ln -sf ~/dotfiles/ignore ~/.ignore

# lsd
mkdir -p ~/.config/lsd
ln -sf ~/dotfiles/lsd.yaml ~/.config/lsd/config.yaml

# Yabai
ln -sf ~/dotfiles/yabai/yabairc ~/.yabairc
ln -sf ~/dotfiles/yabai/skhdrc ~/.skhdrc

# Clean up symlinks
~/dotfiles/scripts/clean-symlinks.sh

echo "Links updated"
