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

# Ack
ln -sf ~/dotfiles/ackrc ~/.ackrc

# Alacritty
ln -sf ~/dotfiles/alacritty.yml ~/.alacritty.yml

# Profile
ln -sf ~/dotfiles/profile ~/.profile

# Fish
mkdir -p ~/.config/fish/functions
ln -sf ~/dotfiles/fish/config.fish ~/.config/fish/config.fish
ln -sf ~/dotfiles/fish/fkill.fish ~/.config/fish/functions/fkill.fish
ln -sf ~/dotfiles/fish/hostip.fish ~/.config/fish/functions/hostip.fish
ln -sf ~/dotfiles/fish/killport.fish ~/.config/fish/functions/killport.fish
ln -sf ~/dotfiles/fish/sudo.fish ~/.config/fish/functions/sudo.fish
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

# Termonad
mkdir -p ~/.config/termonad
ln -sf ~/dotfiles/termonad/termonad.hs ~/.config/termonad/termonad.hs

# Ranger
mkdir -p ~/.config/ranger
ln -sf ~/dotfiles/rc.conf ~/.config/ranger/rc.conf

# Qutebrowser
mkdir -p ~/.config/qutebrowser
ln -sf ~/dotfiles/qutebrowser/config.py ~/.config/qutebrowser/config.py
ln -sf ~/dotfiles/qutebrowser/dracula.py ~/.config/qutebrowser/dracula.py

# bin
mkdir -p ~/bin
ln -sf ~/dotfiles/bin/asciinemagif ~/bin/asciinemagif
ln -sf ~/dotfiles/bin/devour ~/bin/devour
ln -sf ~/dotfiles/bin/emoji ~/bin/emoji
ln -sf ~/dotfiles/bin/explainshell ~/bin/explainshell
ln -sf ~/dotfiles/bin/ghc ~/bin/ghc
ln -sf ~/dotfiles/bin/ghci ~/bin/ghci
ln -sf ~/dotfiles/bin/gifgen ~/bin/gifgen
ln -sf ~/dotfiles/bin/git-remote-setup-fork ~/bin/git-remote-setup-fork
ln -sf ~/dotfiles/bin/gitignore ~/bin/gitignore
ln -sf ~/dotfiles/bin/hlint-brittany ~/bin/hlint-brittany
ln -sf ~/dotfiles/bin/hoogle-rofi ~/bin/hoogle-rofi
ln -sf ~/dotfiles/bin/i3-battery-popup ~/bin/i3-battery-popup
ln -sf ~/dotfiles/bin/image-info ~/bin/image-info
ln -sf ~/dotfiles/bin/image-preview ~/bin/image-preview
ln -sf ~/dotfiles/bin/init-xmonad ~/bin/init-xmonad
ln -sf ~/dotfiles/bin/lastpass-rofi ~/bin/lastpass-rofi
ln -sf ~/dotfiles/bin/lock ~/bin/lock
ln -sf ~/dotfiles/bin/passmenu-otp ~/bin/passmenu-otp
ln -sf ~/dotfiles/bin/script-rofi ~/bin/script-rofi
ln -sf ~/dotfiles/bin/stack-test ~/bin/stack-test
ln -sf ~/dotfiles/bin/todo ~/bin/todo
ln -sf ~/dotfiles/bin/touchpad-toggle ~/bin/touchpad-toggle
ln -sf ~/dotfiles/bin/webcam ~/bin/webcam

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

# slack-term
mkdir -p ~/.config/slack-term
ln -sf ~/dotfiles/slack-term ~/.config/slack-term/config

# markdownlint
ln -sf ~/dotfiles/markdownlintrc ~/.markdownlintrc

# slack-term
mkdir -p ~/.config/bottom
ln -sf ~/dotfiles/bottom.toml ~/.config/bottom/bottom.toml

# doom emacs
mkdir -p ~/.doom.d
ln -sf ~/dotfiles/doom-emacs/config.el ~/.doom.d/config.el
ln -sf ~/dotfiles/doom-emacs/init.el ~/.doom.d/init.el
ln -sf ~/dotfiles/doom-emacs/packages.el ~/.doom.d/packages.el

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

# Purty
# Use purty binary directly, avoid node spawning a process (performance)
ln -sf ~/.npm-global/lib/node_modules/purty/bin/linux/purty ~/bin/purty

echo "Links updated"
