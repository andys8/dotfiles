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
set -g EDITOR vim

# Cursor speed
xset r rate 200 80

# Key bindings: vim and default emacs
if status --is-interactive
  fish_hybrid_key_bindings
end

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
  abbr ls 'exa -a'
  abbr ll 'exa -al'
  abbr update 'sudo apt-get update; and sudo apt-get upgrade'
  abbr .. 'cd ..'
  abbr ... 'cd ../..'
  abbr ackf 'ack -f | ack'
  abbr tl 'toilet -t -F border -f future --gay'

end

