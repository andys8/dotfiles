# dotfiles

This repository needs to be cloned in home directory (`~`). The `install.sh` script checks pre- and post-conditions, links files and installs/updates programs.

```sh
cd ~
git clone https://github.com/andys8/dotfiles.git
cd dotfiles
./install.sh
```

## Local files (depending on machine)

### `~/.gitconfig.user`

Local default git user configuration with an example for conditional includes.

```text
[user]
    name = andys8
    email = andys8@users.noreply.github.com

[includeIf "gitdir:~/dev/repository/work/"]
    path = ~/.gitconfig.work
```

### `~/bin/startup.sh`

Local script executed on machine startup

```bash
#!/bin/bash
dropbox start &
duplicati &
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

Install [`andys8/st`](https://github.com/andys8/st)

#### Add st to terminals and set as default

```shell
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/st 80
sudo update-alternatives --config x-terminal-emulator
```

## Xmonad

`/usr/share/xsessions/xmonad.desktop` for lightdm

```text
[Desktop Entry]
Type=Application
Encoding=UTF-8
Name=Xmonad
Exec=xmonad
X-GNOME-WMName=Xmonad
X-GNOME-Autostart-Phase=WindowManager
X-GNOME-Provides=windowmanager
X-GNOME-Autostart-Notify=false
```

### `Super + Space` on Ubuntu

Run `ibus-setup` and delete shortcut to remove conflict with keybinding.

## Qutebrowser

### Make Qutebrowser the default web browser

```shell
xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop
```

### Download dictionaries for spell checking

```sh
git clone https://github.com/qutebrowser/qutebrowser.git
./qutebrowser/scripts/dictcli.py install en-US
./qutebrowser/scripts/dictcli.py install de-DE
```

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

