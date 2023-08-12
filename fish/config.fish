# Load Oh My Fish configuration.
source $OMF_PATH/init.fish

# Source .profile if missing
if [ "$PROFILE_SOURCED" != "1" ]
  fenv source ~/.profile
  set PROFILE_SOURCED 1
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
set -U theme_display_git_ahead_verbose no
set -U theme_display_git_default_branch no
set -U theme_display_git_dirty yes
set -U theme_display_git_dirty_verbose no
set -U theme_display_git_stashed_verbose no
set -U theme_display_hg no
set -U theme_display_k8s_context no
set -U theme_display_nix yes
set -U theme_display_node no
set -U theme_display_nvm no
set -U theme_display_ruby no
set -U theme_display_vagrant no
set -U theme_display_vi no
set -U theme_git_default_branches master main develop
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

# Keybindings in visual mode
bind --user --mode visual --sets-mode insert i force-repaint end-selection
bind --user --mode visual --sets-mode insert s kill-selection end-selection repaint-mode

# Keybindings QMK like: Ctrl-Space
bind --user --mode insert --key nul backward-delete-char

# Keybinding: Ctrl-Backspace
bind --user --mode insert \cH backward-kill-path-component

# Cursor speed
xset r rate 200 80 2> /dev/null &

# zoxide init
zoxide init fish | source

# nix-shell using fix via any-nix-shell
function nix-shell
  any-nix-shell fish | source
  nix-shell $argv
end

# Aliases
abbr -a -- .. 'cd ..'
abbr -a -- ... 'cd ../..'
abbr -a -- .... 'cd ../../..'
abbr -a -- ..... 'cd ../../../..'
abbr -a -- ackf 'ack -f | ack'
abbr -a -- agc 'sudo apt autoclean'
abbr -a -- agi 'sudo apt install'
abbr -a -- agr 'sudo apt remove'
abbr -a -- agu 'sudo apt update && sudo apt upgrade'
abbr -a -- benchmark hyperfine
abbr -a -- calc wcalc
abbr -a -- cheat cht.sh
abbr -a -- colorpicker gcolor3
abbr -a -- copy-screenshot 'cp (ls -t ~/Pictures/screenshots/screenshot-* | head -n1) .'
abbr -a -- dco docker-compose
abbr -a -- de 'trans en:de'
abbr -a -- du duf
abbr -a -- e 'fzf | xargs -ro vim'
abbr -a -- en 'trans de:en'
abbr -a -- fvim 'fzf | xargs -ro vim'
abbr -a -- g git
abbr -a -- ga 'git add'
abbr -a -- gaa 'git add .'
abbr -a -- gb 'git brunch'
abbr -a -- gc 'git commit'
abbr -a -- gca 'git commit -a'
abbr -a -- gcd 'git checkout develop'
abbr -a -- gcm 'git checkout master'
abbr -a -- gco 'git checkout'
abbr -a -- gcp 'git cherry-pick'
abbr -a -- gd 'git diff'
abbr -a -- gds 'git diff --staged'
abbr -a -- gf 'git fetch --all'
abbr -a -- gi git
abbr -a -- gitb 'git b'
abbr -a -- gl 'git log'
abbr -a -- gm 'git merge'
abbr -a -- gp 'git push'
abbr -a -- gpl 'git pull'
abbr -a -- gr 'git remote'
abbr -a -- gra 'git rebase --abort'
abbr -a -- grc 'git rebase --continue'
abbr -a -- grd 'git rebase -i origin/develop'
abbr -a -- grm 'git rebase -i origin/master'
abbr -a -- gru 'git rebase -i upstream/master'
abbr -a -- gs 'git status'
abbr -a -- gst 'git stash'
abbr -a -- gti git
abbr -a -- imp image-preview
abbr -a -- j z
abbr -a -- lg lazygit
abbr -a -- ll 'lsd -Al --date relative --group-dirs first'
abbr -a -- lll 'lsd -Al --date relative --timesort --reverse'
abbr -a -- ls lsd
abbr -a -- mirror 'autorandr common'
abbr -a -- ng 'nix-env -qaP | grep'
abbr -a -- ns nix-search
abbr -a -- ntop nethogs
abbr -a -- open 'devour xdg-open'
abbr -a -- paci 'sudo pacman -S'
abbr -a -- pacr 'sudo pacman -Rs'
abbr -a -- pacs 'pacman -Ss'
abbr -a -- pacu 'sudo pacman -Syu'
abbr -a -- r ranger
abbr -a -- rm trash
abbr -a -- stacktest stack-test
abbr -a -- stb 'stack build --test --no-run-tests'
abbr -a -- stbw 'stack build --test --no-run-tests --file-watch'
abbr -a -- sti 'stack build --test --no-run-tests --copy-bins'
abbr -a -- stt stack-test
abbr -a -- suspend 'systemctl suspend'
abbr -a -- t tig
abbr -a -- td todo
abbr -a -- tdls 'ranger ~/Documents/todo'
abbr -a -- tdt 'todo tomorrow'
abbr -a -- tdy 'todo yesterday'
abbr -a -- timestamp 'date +%s%3N'
abbr -a -- tl 'toilet -t -F border -f future --gay'
abbr -a -- todols 'todo ls'
abbr -a -- toggle-touchpad touchpad-toggle
abbr -a -- usb 'ranger /run/media/(whoami)'
abbr -a -- uuid uuidgen
abbr -a -- v vim
abbr -a -- vi vim
abbr -a -- vimg 'vim (git diff --name-only)'
abbr -a -- vmi vim
abbr -a -- weather 'curl https://wttr.in'
abbr -a -- wttr 'curl https://wttr.in'
abbr -a -- öö ll
