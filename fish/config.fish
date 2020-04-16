# Load Oh My Fish configuration.
source $OMF_PATH/init.fish

# Source .profile if missing
if not test -n "$PROFILE_SOURCED"
  # .profile will set .PROFILE_SOURCED therefore no loop
  bash -c "source ~/.profile; fish"
end

# Keybindings
bind --user --mode visual --sets-mode insert i force-repaint end-selection
bind --user --mode visual --sets-mode insert s kill-selection end-selection repaint-mode

# Cursor speed
xset r rate 200 80 2> /dev/null &

# Autojump
[ -f "$HOME/.nix-profile/share/autojump/autojump.fish" ];
  and source "$HOME/.nix-profile/share/autojump/autojump.fish"

# sudo !!
function sudo
  if test "$argv" = !!
    eval command sudo $history[1]
  else
    command sudo $argv
  end
end

