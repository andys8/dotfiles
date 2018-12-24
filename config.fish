# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
source $OMF_PATH/init.fish

function fish_greeting
end

# Bobthefish theme options
set -g theme_nerd_fonts yes
set -g theme_display_vagrant no
set -g theme_display_docker_machine no
set -g theme_display_hg no
set -g theme_display_ruby no
set -g theme_display_date no
set -g theme_display_vi yes

# Autojump
[ -f /usr/share/autojump/autojump.fish ]; and source /usr/share/autojump/autojump.fish

# Environment
set -gx PATH $HOME/.local/bin $PATH

# Cursor speed
xset r rate 200 80

# Key bindings: vim and default emacs
if status --is-interactive
  fish_hybrid_key_bindings
end
