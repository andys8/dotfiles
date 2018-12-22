# dotfiles

This repository will be checked out in home dir.

```sh
./install.sh
```

## Alacritty

### DPI

Add `WINIT_HIDPI_FACTOR=1.0` to `/etc/environment`

### Set as default terminal

```shell
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/local/bin/alacritty 80

sudo update-alternatives --config x-terminal-emulator
```