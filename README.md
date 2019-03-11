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
```

### `~/bin/startup.sh`

Local script executed on machine startup

```bash
#!/bin/bash
dropbox start &
duplicati &
```

## Alacritty

### DPI

Add `WINIT_HIDPI_FACTOR=1.0` to `/etc/environment`

### Set as default terminal

```shell
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/bin/alacritty 80
sudo update-alternatives --config x-terminal-emulator
```
