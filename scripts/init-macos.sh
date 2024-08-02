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

# animations
defaults write -g NSScrollViewRubberbanding -int 0
defaults write -g NSAutomaticWindowAnimationsEnabled -bool false
defaults write -g NSScrollAnimationEnabled -bool false
defaults write -g NSWindowResizeTime -float 0.001
defaults write -g QLPanelAnimationDuration -float 0
defaults write -g NSScrollViewRubberbanding -bool false
defaults write -g NSDocumentRevisionsWindowTransformAnimation -bool false
defaults write -g NSToolbarFullScreenAnimationDuration -float 0
defaults write -g NSBrowserColumnAnimationSpeedMultiplier -float 0
defaults write com.apple.dock autohide-time-modifier -float 0
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock expose-animation-duration -float 0
defaults write com.apple.dock springboard-show-duration -float 0
defaults write com.apple.dock springboard-hide-duration -float 0
defaults write com.apple.dock springboard-page-duration -float 0
defaults write com.apple.finder DisableAllAnimations -bool true
defaults write com.apple.Mail DisableSendAnimations -bool true
defaults write com.apple.Mail DisableReplyAnimations -bool true
defaults write NSGlobalDomain NSWindowResizeTime .001
defaults write com.apple.dock expose-animation-duration -int 0
defaults write com.apple.dock expose-animation-duration -float 0.1
killall Dock
