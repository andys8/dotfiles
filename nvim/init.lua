-- Load Cursor-specific configuration if in Cursor environment
local cursor_config_path = vim.fn.stdpath('config') .. '/cursor.lua'
if vim.fn.filereadable(cursor_config_path) == 1 then
  dofile(cursor_config_path)
end

-- Create lua directory if it doesn't exist
local lua_dir = vim.fn.stdpath('config') .. '/lua'
if vim.fn.isdirectory(lua_dir) == 0 then
  vim.fn.mkdir(lua_dir, 'p')
end

-- Basic Settings
vim.g.mapleader = ','
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'
vim.opt.termguicolors = true

-- Tab settings
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 0
vim.opt.shiftwidth = 4

-- Line numbers and visual settings
vim.opt.number = true
vim.opt.ruler = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.scrolloff = 3
vim.opt.hidden = true
vim.opt.laststatus = 2
vim.opt.clipboard = "unnamedplus"

-- No backups
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false

-- Performance settings
vim.opt.updatetime = 300
vim.opt.timeoutlen = 500
vim.opt.ttimeoutlen = 20
vim.opt.lazyredraw = true

-- Skip the rest of normal Neovim configuration if in Cursor
if vim.g.vscode then
  return
end

-- Load plugins
require('plugins')

-- Load configurations
local status_ok, telescope_config = pcall(require, 'telescope_config')
if status_ok then
  telescope_config.setup()
end

local status_ok, lsp_config = pcall(require, 'lsp_config')
if status_ok then
  lsp_config.setup()
end

local status_ok, completion_config = pcall(require, 'completion_config')
if status_ok then
  completion_config.setup()
end

-- Configure nvim-tree
local nvim_tree_status_ok, nvim_tree = pcall(require, 'nvim-tree')
if nvim_tree_status_ok then
  nvim_tree.setup {
    disable_netrw = true,
    hijack_netrw = true,
    respect_buf_cwd = true,
    update_cwd = true,
    view = {
      width = 30,
    }
  }

  -- NERDTree-like keybindings for nvim-tree
  vim.keymap.set('n', '<leader>n', '<cmd>NvimTreeFindFileToggle<CR>', { silent = true, desc = 'Toggle NvimTree' })
  vim.keymap.set('n', '<leader>N', '<cmd>NvimTreeToggle<CR>', { silent = true, desc = 'Toggle NvimTree' })
end

-- Configure gitsigns
local gitsigns_status_ok, gitsigns = pcall(require, 'gitsigns')
if gitsigns_status_ok then
  gitsigns.setup {
    signs = {
      add = { text = '✚' },
      change = { text = '➜' },
      delete = { text = '✘' },
      topdelete = { text = '✘' },
      changedelete = { text = '±' },
    },
    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns

      -- Git navigation keymaps
      vim.keymap.set('n', '<leader>m', gs.next_hunk, { buffer = bufnr, desc = 'Next git hunk' })
      vim.keymap.set('n', '<leader>M', gs.preview_hunk, { buffer = bufnr, desc = 'Preview git hunk' })
    end
  }
end

-- Configure lualine (status line)
local lualine_status_ok, lualine = pcall(require, 'lualine')
if lualine_status_ok then
  lualine.setup {
    options = {
      theme = 'dracula',
      icons_enabled = true,
    },
    sections = {
      lualine_c = {
        {
          'filename',
          path = 1, -- Show relative path
        }
      }
    }
  }
end

-- Key mappings for standard Neovim (when not using plugin-specific bindings)

-- Window navigation
vim.keymap.set('n', '<C-h>', '<C-w>h', { desc = 'Navigate to left window' })
vim.keymap.set('n', '<C-j>', '<C-w>j', { desc = 'Navigate to bottom window' })
vim.keymap.set('n', '<C-k>', '<C-w>k', { desc = 'Navigate to top window' })
vim.keymap.set('n', '<C-l>', '<C-w>l', { desc = 'Navigate to right window' })

-- Split windows
vim.keymap.set('n', '<Leader>h', '<cmd>split<CR>', { desc = 'Horizontal split' })
vim.keymap.set('n', '<Leader>v', '<cmd>vsplit<CR>', { desc = 'Vertical split' })

-- Buffer navigation
vim.keymap.set('n', '<leader>z', '<cmd>bp<CR>', { desc = 'Previous buffer' })
vim.keymap.set('n', '<leader>q', '<cmd>bp<CR>', { desc = 'Previous buffer' })
vim.keymap.set('n', '<leader>x', '<cmd>bn<CR>', { desc = 'Next buffer' })
vim.keymap.set('n', '<leader>w', '<cmd>bn<CR>', { desc = 'Next buffer' })
vim.keymap.set('n', '<leader>c', '<cmd>bd<CR>', { desc = 'Close buffer' })

-- Clear search highlighting
vim.keymap.set('n', '<leader><space>', '<cmd>noh<CR>', { silent = true, desc = 'Clear search highlight' })

-- Format current file
vim.keymap.set('n', '<leader>p', function()
  vim.lsp.buf.format({ async = true })
end, { desc = 'Format current file' })

-- Exit insert mode with jj or jk
vim.keymap.set('i', 'jj', '<ESC>', { desc = 'Exit insert mode' })
vim.keymap.set('i', 'jk', '<ESC>', { desc = 'Exit insert mode' })

-- Save file
vim.keymap.set('n', '<C-s>', '<cmd>w<CR>', { desc = 'Save file' })
vim.keymap.set('i', '<C-s>', '<ESC><cmd>w<CR>', { desc = 'Exit insert mode and save file' })

-- Copy for map when selected
vim.keymap.set('v', 'y', '"+y', { desc = 'Copy to system clipboard' })

-- Yank consistency with other commands
vim.keymap.set('n', 'Y', 'y$', { desc = 'Yank to end of line' })

-- Visual mode indentation keeps selection
vim.keymap.set('v', '<', '<gv', { desc = 'Indent left and keep selection' })
vim.keymap.set('v', '>', '>gv', { desc = 'Indent right and keep selection' })

-- Move visual block
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", { desc = 'Move selected block down' })
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", { desc = 'Move selected block up' })

-- Make paste in visual mode not copy the replaced text
vim.keymap.set('x', 'p', '"_dP', { desc = 'Paste without yanking replaced text' })

-- Custom commands
vim.api.nvim_create_user_command('Config', 'e $MYVIMRC', { desc = 'Edit Neovim config' })
vim.api.nvim_create_user_command('Reload', 'source $MYVIMRC', { desc = 'Reload Neovim config' })
vim.api.nvim_create_user_command('ConfigPlugins', 'e ' .. vim.fn.stdpath('config') .. '/lua/plugins.lua', { desc = 'Edit plugins config' })

-- Abbreviations similar to the original vim config
vim.cmd([[
  cnoreabbrev W! w!
  cnoreabbrev Q! q!
  cnoreabbrev Qall! qall!
  cnoreabbrev Wq wq
  cnoreabbrev Wa wa
  cnoreabbrev wQ wq
  cnoreabbrev WQ wq
  cnoreabbrev W w
  cnoreabbrev Q q
  cnoreabbrev Qall qall
]])

-- Fix opening files with sudo
vim.cmd([[
  cnoreabbrev w!! w !sudo tee % >/dev/null
]])

-- Add colorscheme command
vim.cmd([[
  command! -nargs=0 Dracula colorscheme dracula
  command! -nargs=0 Nord colorscheme nord
]])

-- Load themes (preferring dracula as in original config)
vim.cmd([[
  try
    colorscheme dracula
  catch
    try
      colorscheme nord
    catch
      colorscheme default
    endtry
  endtry
]])

-- Load local config if it exists
vim.cmd([[
if filereadable(expand("~/.config/nvim/local.lua"))
  source ~/.config/nvim/local.lua
endif
]])
