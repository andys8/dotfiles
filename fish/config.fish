# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
source $OMF_PATH/init.fish

# Source .profile if missing
if not test -n "$PROFILE_SOURCED"
  fenv source ~/.profile
end

# Cursor speed
xset r rate 200 80

# Autojump
[ -f "$HOME/.nix-profile/share/autojump/autojump.fish" ];
  and source "$HOME/.nix-profile/share/autojump/autojump.fish"

# Key bindings: vim and default emacs
if status --is-interactive
  set -U fish_key_bindings fish_hybrid_key_bindings
end

# sudo !!
function sudo
  if test "$argv" = !!
    eval command sudo $history[1]
  else
    command sudo $argv
  end
end

