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

### Change shell to fish

```shell
bash -c 'chsh -s $(chsh -l | grep -m 1 fish)'
```

### Fish theme

Open config with `fish_config` and set theme to `dracula` in Web-UI.

## Terminal

### Alacritty

#### DPI

Add `WINIT_HIDPI_FACTOR=1.0` to `/etc/environment`

### Add alacritty to terminals

```shell
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/bin/alacritty 80
```

### st (suckless-terminal)

Fork of st: [`andys8/st`](https://github.com/andys8/st)

#### Add st to terminals and set as default

```shell
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/st 80
sudo update-alternatives --config x-terminal-emulator
```

## Qutebrowser

### Default browser

```shell
xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop
xdg-settings set default-web-browser chromium.desktop # or stick with chromium
xdg-mime default chromium.desktop x-scheme-handler/https
xdg-mime default chromium.desktop x-scheme-handler/http
```

### Spell checking

Download dictionaries for spell checking

```sh
git clone https://github.com/qutebrowser/qutebrowser.git && cd qutebrowser
./scripts/dictcli.py install en-US
./scripts/dictcli.py install de-DE
# or with virtualenv (if wrong directory)
./.venv/bin/python3 ./scripts/dictcli.py install en-US
```

Might be necessary to copy to `~/.local/share/qutebrowser/qtwebengine_dictionaries`

## Time

Enable time synchronization with `timedatectl set-ntp true`.

## Git

Set credentials in `~/.netrc`

## `sudo`

Open `sudo visudo` and add these lines to enable asterisks and silly feedback.

```text
Defaults insults
Defaults pwfeedback
```

- `Defaults secure_path` is **not set** to keep `$PATH` and access to user installed binaries with sudo.
- `Defaults env_keep += "HOME"` keeps the home directory and uses configuration files.

## Pacman

Enable in `/etc/pacman.conf`:

```text
Color
ILoveCandy
VerbosePkgLists
```

## Theme

Install `xcursor-breeze` and [`Ant-Dracula`](https://github.com/EliverLara/Ant-Dracula) theme. Configure with `lxappearance`.

![stats](https://static.scarf.sh/a.png?x-pxid=8c48fb17-fc67-4552-90d5-7cfb267fd0c8)
