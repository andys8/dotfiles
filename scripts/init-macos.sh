#!/bin/bash

# dock
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 1000
defaults write com.apple.dock no-bouncing -bool true
killall Dock

# key repeat
defaults write -g initialKeyRepeat -int 13
defaults write -g KeyRepeat -int 1

# scroll direction
defaults write -g com.apple.swipescrolldirection -boolean NO

# finder
defaults write com.apple.finder DisableAllAnimations -bool true
