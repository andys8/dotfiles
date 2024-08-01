#!/bin/bash
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 1000
defaults write com.apple.dock no-bouncing -bool TRUE
killall Dock
