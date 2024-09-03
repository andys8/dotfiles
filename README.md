# dotfiles

Clone this repository in your home directory (`~`). The `install.sh` script checks pre- and post-conditions, links files and installs/updates programs.

```sh
cd ~
git clone https://github.com/andys8/dotfiles.git
cd dotfiles
./install.sh
```

## Machine specific configuration

### `~/bin/startup.sh`

Local script executed on machine startup

```bash
#!/bin/bash
[ -z "$(pgrep dropbox)" ] && dropbox start &
[ -z "$(pgrep -f duplicati)" ] && duplicati &
# ubuntu only
[ -z "$(pgrep pulseaudio)" ] && pulseaudio --daemonize
```

```bash
#!/bin/bash
[ -z "$(pgrep -f slack)" ] && slack --startup &
[ -z "$(pgrep -f gcal-notifier-kotlin-gtk)" ] && gcal-notifier-kotlin-gtk &
[ -z "$(pgrep -f hasmail)" ] || killall hasmail
hasmail &
```

### `.hasmailrc`

```text
click=xdg-open https://mail.google.com

[gmail]
hostname=imap.gmail.com:993
username=<email>
password=lpass show -p personal/gmail-hasmail
```

### `~/.gitconfig.machine`

Local default git user configuration with an example for conditional includes.

```text
[user]
    name = andys8
    email = andys8@users.noreply.github.com

[includeIf "gitdir:~/dev/repository/work/"]
    path = ~/.gitconfig.work
```

### `~/.gitconfig.work`

```text
[user]
    name = andreas
    email = andreas@work.com
[url "ssh://git@github.com/"]
    insteadOf = https://andreas-work@github.com/
```

### `~/.profile.machine`

Contains per system environment variables (optional).

```sh
export PATH=/home/user/example-path:$PATH
export SLACK_TOKEN=example-token
```

## Shell

### Fish theme

Open config with `fish_config` and set theme to `dracula` in Web-UI.

## Git

Set credentials in `~/.netrc`

## Drag and drop (terminal)

Currently using <https://github.com/Wevah/dragterm> (drag binary). Alternative `open -R`.

## Keyboard: PC ISO-DE on Mac

Configure German PC keyboard layout (modified with Ukelele)

```shell
sudo cp keyboard/isode.keylayout /Library/Keyboard\ Layouts/
```

### Mapping

Right command behaves like right option

```shell
cp keyboard/com.local.KeyRemapping.plist ~/Library/LaunchAgents/
```

Source: <https://github.com/skrysmanski/windows-pc-keyboard-layout-mac>

### Capslock

Manually map to `Fn/Globe` in keyboard settings

## Vim

Make sure vim of homebrew is installed (python3) and linked.

<https://gist.github.com/SofijaErkin/d428dcbf6a651673af63f45e851783cf>

## PATH

Add `/Users/username/bin` to `/etc/paths` to be able to overwrite system binaries.
