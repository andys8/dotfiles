# dotfiles

This repository will be checked out in home dir.

```sh
./install.sh
```

## Local files

### `~/.gitconfig.user`

Local default git user configuration

```
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

```bash
chsh -s $(which fish)
```

### Fish theme

Set theme to `dracula` in web ui.

```shell
fish_config
```

## Alacritty

### DPI

Add `WINIT_HIDPI_FACTOR=1.0` to `/etc/environment`

### Set as default terminal

```shell
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/bin/alacritty 80
sudo update-alternatives --config x-terminal-emulator
```

## st (suckless-terminal)

Install [`andys8/st`](https://github.com/andys8/st)

### Set as default terminal

```shell
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/st 80
sudo update-alternatives --config x-terminal-emulator
```

## Python

Select default version:

```shell
sudo update-alternatives --config python
```

If missing, add with:

```shell
sudo update-alternatives --install /usr/bin/python python /usr/bin/python2.7 2
```

## Xmonad

`/usr/share/xsessions/xmonad.desktop` for lightdm

```
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

## Time

Arch: Enable time synchronization with `timedatectl set-ntp true`.
