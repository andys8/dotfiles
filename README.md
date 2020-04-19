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
# ubuntu, but not arch
[ -z "$(pgrep pulseaudio)" ] && pulseaudio --daemonize
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

### `~/.profile.machine`

Contains per system environment variables (optional).

```sh
export PATH=/home/user/example-path:$PATH
export SLACK_TOKEN=example-token
```

### `~/.vimrc.machine`

Machine specific vim configuration

```vim
Plug 'https://account@github.com/org/plugin', { 'for' : 'language' }
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

## Xmonad

`lightdm` is expected. `/usr/share/xsessions/xmonad.desktop` will be created if not existing.

### `Super + Space` on Ubuntu

Run `ibus-setup` and delete shortcut to remove conflict with keybinding.

## Qutebrowser

### Make Qutebrowser the default web browser

```shell
xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop
```

### Spell checking

Download dictionaries for spell checking

```sh
git clone https://github.com/qutebrowser/qutebrowser.git
./qutebrowser/scripts/dictcli.py install en-US
./qutebrowser/scripts/dictcli.py install de-DE
```

Might be necessary to copy to `~/.local/share/qutebrowser/qtwebengine_dictionaries`

## Time

Arch: Enable time synchronization with `timedatectl set-ntp true`.

## Git

Set credentials in `~/.netrc`

## Python

### Pip

Arch: Install `sudo pacman -Syu python-pip`

### Select default python version

```shell
sudo update-alternatives --config python
```

If missing, add with:

```shell
sudo update-alternatives --install /usr/bin/python python /usr/bin/python2.7 2
```

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
