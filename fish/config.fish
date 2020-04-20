# Load Oh My Fish configuration.
source $OMF_PATH/init.fish

# Source .profile if missing
if not test -n "$PROFILE_SOURCED"
  # .profile will set .PROFILE_SOURCED therefore no loop
  bash -c "source ~/.profile; fish"
end

# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -Ux OMF_PATH "$XDG_DATA_HOME/omf"
  or set -Ux OMF_PATH "$HOME/.local/share/omf"

# Key bindings: vim and default emacs
set -U fish_key_bindings fish_hybrid_key_bindings

# Bobthefish theme options
set -U theme_color_scheme dracula
set -U theme_display_cmd_duration no
set -U theme_display_date no
set -U theme_display_docker_machine no
set -U theme_display_hg no
set -U theme_display_k8s_context no
set -U theme_display_nvm no
set -U theme_display_ruby no
set -U theme_display_vagrant no
set -U theme_display_vi yes
set -U theme_git_worktree_support no
set -U theme_nerd_fonts yes
set -U theme_show_exit_status no

# Colors in man pages / less
set -Ux LESS_TERMCAP_mb (set_color -o magenta)
set -Ux LESS_TERMCAP_md (set_color -o brgreen)
set -Ux LESS_TERMCAP_so (set_color -b blue bryellow)
set -Ux LESS_TERMCAP_us (set_color -u brred)
set -Ux LESS_TERMCAP_me (set_color normal)
set -Ux LESS_TERMCAP_ue (set_color normal)
set -Ux LESS_TERMCAP_se (set_color normal)

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

