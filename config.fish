# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
source $OMF_PATH/init.fish

# Environment
set -gx EDITOR vim

# No greeting
function fish_greeting
end

# Bobthefish theme options
set -g theme_color_scheme dracula
set -g theme_nerd_fonts yes
set -g theme_display_vagrant no
set -g theme_display_docker_machine no
set -g theme_display_hg no
set -g theme_display_ruby no
set -g theme_display_date no
set -g theme_display_vi yes

# Autojump
[ -f /usr/share/autojump/autojump.fish ]; and source /usr/share/autojump/autojump.fish

# fnm
fnm env --multi | source

# Cursor speed
xset r rate 200 80

# Key bindings: vim and default emacs
if status --is-interactive
  fish_hybrid_key_bindings
end

# sudo !!
function sudo
  if test "$argv" = !!
    eval command sudo $history[1]
  else
    command sudo $argv
  end
end

# Fix for LD_PRELOAD not found in sdk and screenfetch
set -x LD_PRELOAD "/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0"

# Colors in man pages / less
set -x LESS_TERMCAP_mb (set_color -o magenta)
set -x LESS_TERMCAP_md (set_color -o brgreen)
set -x LESS_TERMCAP_so (set_color -b blue bryellow)
set -x LESS_TERMCAP_us (set_color -u brred)
set -x LESS_TERMCAP_me (set_color normal)
set -x LESS_TERMCAP_ue (set_color normal)
set -x LESS_TERMCAP_se (set_color normal)

# Abbreviations
if not set -q abbrs_initialized
  set -U abbrs_initialized
  echo 'Fish: Setting abbreviations'

  abbr r 'ranger'
  abbr v 'vim'
  abbr t 'tig'
  abbr n 'npm'
  abbr nr 'npm run'
  abbr g 'git'
  abbr gi 'git'
  abbr gti 'git'
  abbr ga 'git add'
  abbr gc 'git commit -m'
  abbr gco 'git checkout'
  abbr gd 'git diff'
  abbr gds 'git diff --staged'
  abbr gf 'git fetch'
  abbr gl 'git log'
  abbr gm 'git merge'
  abbr gp 'git push'
  abbr gpl 'git pull'
  abbr gr 'git remote'
  abbr gs 'git status'
  abbr gst 'git stash'
  abbr agi 'sudo apt-get install'
  abbr agr 'sudo apt-get remove'
  abbr ll 'exa -al'
  abbr update 'sudo apt-get update; and sudo apt-get upgrade'
  abbr .. 'cd ..'
  abbr ... 'cd ../..'
  abbr ackf 'ack -f | ack'
  abbr tl 'toilet -t -F border -f future --gay'
  abbr cheat 'curl cheat.sh/'

end

