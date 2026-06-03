# Native fish environment setup.
#
# Replaces `fenv source ~/.profile`, which spawned a bash subprocess on every
# new shell (~230ms, the bulk of cold-start cost since a fresh GUI window never
# inherits PROFILE_SOURCED). ~/.profile stays the source of truth for bash/login
# shells; keep the two in sync when adding PATH entries or env vars.

# Homebrew (equivalent of `brew shellenv`)
set -gx HOMEBREW_PREFIX /opt/homebrew
set -gx HOMEBREW_CELLAR /opt/homebrew/Cellar
set -gx HOMEBREW_REPOSITORY /opt/homebrew
set -q MANPATH; or set MANPATH ''
not contains -- /opt/homebrew/share/man $MANPATH
    and set -gx MANPATH /opt/homebrew/share/man $MANPATH
set -q INFOPATH; or set INFOPATH ''
not contains -- /opt/homebrew/share/info $INFOPATH
    and set -gx INFOPATH /opt/homebrew/share/info $INFOPATH

# PATH (mirrors ~/.profile). Listed low-to-high priority; each existing dir is
# prepended, so /opt/homebrew/bin ends up first. Guarded so nested shells that
# already inherit these entries don't duplicate them.
for dir in $HOME/.bun/bin $HOME/.npm-global/bin $HOME/.local/bin \
        $HOME/bin $HOME/dotfiles/bin $HOME/dotfiles/scripts \
        /opt/homebrew/sbin /opt/homebrew/bin
    test -d $dir; and not contains -- $dir $PATH
        and set -gx PATH $dir $PATH
end

# Environment
if not contains -- $HOME/.share $XDG_DATA_DIRS
    set -q XDG_DATA_DIRS[1]
        and set -gx XDG_DATA_DIRS $HOME/.share $XDG_DATA_DIRS
        or set -gx XDG_DATA_DIRS $HOME/.share /usr/local/share/ /usr/share/
end
set -gx N_PREFIX $HOME/.local
set -gx EDITOR vim
set -gx VISUAL vim
set -gx GIT_EDITOR vim
set -gx RANGER_DEVICONS_SEPARATOR "  "

# Marker kept for anything that still checks it (previously set by ~/.profile)
set -gx PROFILE_SOURCED 1
