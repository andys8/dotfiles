# Dotfiles

Personal dotfiles repo, lives at `~/dotfiles`.

## Structure

- `bin/` - custom scripts, added to `$PATH` via `~/.profile`
- `fish/` - fish shell config and functions. Functions in `fish/*.fish` are symlinked into `~/.config/fish/functions/`
- `scripts/` - setup and maintenance scripts
- `yabai/` - yabai window manager and skhd hotkey config
- `git/` - git config templates and hooks
- `nvim/`, `vimrc*` - editor configs

## Symlinks

All config files are symlinked into their expected locations by `scripts/link.sh`. When adding a new config file or fish function, register it in `link.sh` so `scripts/install.sh` picks it up.

Fish functions specifically go to `~/.config/fish/functions/<name>.fish`.

## Formatting

Run `scripts/format-dotfiles.sh` before committing. It runs:

- `shfmt -w -i 4` on shell scripts
- `prettier` on markdown, JSON, and YAML files

## Fish shell

- Shell: fish with Oh My Fish
- Fish functions that need to affect the parent shell (like `cd`) must be fish functions, not standalone scripts
- Abbreviations are defined in `fish/config.fish`
