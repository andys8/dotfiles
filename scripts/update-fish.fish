#!/usr/bin/fish

omf --version
if [ $status -ne 0 ]
  echo "Install omf"
  curl -L https://get.oh-my.fish | fish
end

omf update

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

# Colors in man pages / less
set -Ux LESS_TERMCAP_mb (set_color -o magenta)
set -Ux LESS_TERMCAP_md (set_color -o brgreen)
set -Ux LESS_TERMCAP_so (set_color -b blue bryellow)
set -Ux LESS_TERMCAP_us (set_color -u brred)
set -Ux LESS_TERMCAP_me (set_color normal)
set -Ux LESS_TERMCAP_ue (set_color normal)
set -Ux LESS_TERMCAP_se (set_color normal)

