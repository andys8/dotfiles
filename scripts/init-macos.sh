#!/bin/bash

# key repeat
defaults write -g InitialKeyRepeat -int 12
defaults write -g KeyRepeat -int 1
defaults write -g ApplePressAndHoldEnabled -bool false

# scroll direction
defaults write -g com.apple.swipescrolldirection -boolean NO

# finder
defaults write com.apple.finder DisableAllAnimations -bool true

# hot corners
defaults write com.apple.dock wvous-tl-corner -int 0
defaults write com.apple.dock wvous-tr-corner -int 0
defaults write com.apple.dock wvous-bl-corner -int 0
defaults write com.apple.dock wvous-br-corner -int 0

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

# reduce motion
defaults write com.apple.universalaccess reduceMotion -bool true

# disable click on desktop
defaults write com.apple.WindowManager EnableStandardClickToShowDesktop -bool false

# disable cmd+m minimize
defaults write NSGlobalDomain NSUserKeyEquivalents -dict-add "Minimize" '\0'

# dock
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 1000
defaults write com.apple.dock no-bouncing -bool true
killall Dock || true

# yabai
if command -v yabai &>/dev/null; then
    yabai --stop-service
    killall yabai || true
    yabai --start-service
fi

# skhd
if command -v skhd &>/dev/null; then
    skhd --stop-service
    killall skhd || true
    skhd --start-service
fi

# alacritty daemon (skip if already loaded, it may be hosting open windows)
launchctl print gui/$(id -u)/com.local.alacritty-daemon &>/dev/null ||
    launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.local.alacritty-daemon.plist

# chrome daemon (skip if already loaded, it may be hosting open windows)
launchctl print gui/$(id -u)/com.local.chrome-daemon &>/dev/null ||
    launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.local.chrome-daemon.plist

echo "Init mac os done"
exit 0
