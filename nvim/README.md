# Neovim Configuration

This directory contains a Neovim configuration based on the original Vim setup, with adaptations for Cursor IDE integration.

## Structure

- `init.lua`: Main configuration file
- `cursor.lua`: Cursor-specific keybindings and settings
- `lua/`: Configuration modules
  - `plugins.lua`: Plugin management with packer.nvim
  - `telescope_config.lua`: File searching (equivalent to FZF)
  - `lsp_config.lua`: Language Server Protocol configuration
  - `completion_config.lua`: Auto-completion configuration
  - `multi_cursor_config.lua`: Multi-cursor support

## Key Features

- Leader key set to `,`
- Code navigation similar to the original Vim config
- IDE-like features through LSP and completion

## Key Bindings

| Mapping     | Action                        |
| ----------- | ----------------------------- |
| `,e`        | Search for files              |
| `,f`        | Search for text in files      |
| `,d`        | Jump to definition            |
| `,g`        | Show hover information        |
| `,r`        | Rename symbol                 |
| `,p`        | Format file                   |
| `,s`        | Code actions                  |
| `,a`        | Next diagnostic               |
| `,A`        | Previous diagnostic           |
| `Ctrl+n`    | Multi-cursor functionality    |
| `,n`        | Toggle file explorer          |
| `Ctrl+hjkl` | Navigate between windows      |
| `,h` / `,v` | Split horizontally/vertically |
| `,z` / `,q` | Previous buffer               |
| `,x` / `,w` | Next buffer                   |
| `,c`        | Close buffer                  |

## Installation

The `install.sh` script will create a symlink from `~/.config/nvim` to this directory, so Neovim will use this configuration.

When first launching Neovim, you'll need to install the plugins:

```
:PackerSync
```

## Cursor Integration

When using this configuration with Cursor IDE, the `cursor.lua` file is automatically loaded and maps key bindings to equivalent Cursor commands.
