-- Cursor-specific configuration
-- This file contains settings specifically for Cursor IDE integration

-- At the top of cursor.lua, before any keybinding
vim.g.mapleader = ','
vim.g.maplocalleader = ','

-- Ensure cursor integration
if vim.g.vscode then
  -- Load the VSCode module
  local vscode = require('vscode')

  -- Define Cursor-specific commands
  -- These commands are specific to Cursor functionality

  -- Test basic VSCode action without leader key
  vim.keymap.set('n', '<Space>e', function() vscode.action('workbench.action.quickOpen') end, { silent = true })

  -- Override key bindings with Cursor-specific commands
  vim.keymap.set('n', '<leader>e', function() vscode.action('workbench.action.quickOpen') end, { silent = true })
  vim.keymap.set('n', '<leader>f', function() vscode.action('workbench.action.findInFiles') end, { silent = true })
  vim.keymap.set('n', '<leader>p', function() vscode.action('editor.action.formatDocument') end, { silent = false })
  vim.keymap.set('n', '<leader>d', function() vscode.action('editor.action.revealDefinition') end, { silent = true })
  vim.keymap.set('n', '<leader>g', function() vscode.action('editor.action.showHover') end, { silent = true })
  vim.keymap.set('n', '<leader>r', function() vscode.action('editor.action.rename') end, { silent = true })
  vim.keymap.set('n', '<leader>s', function() vscode.action('editor.action.sourceAction') end, { silent = true })
  vim.keymap.set('n', '<leader>a', function() vscode.action('editor.action.marker.next') end, { silent = true })
  vim.keymap.set('n', '<leader>A', function() vscode.action('editor.action.marker.prev') end, { silent = true })
  vim.keymap.set('n', '<leader>i', function() vscode.action('editor.action.goToImplementation') end, { silent = true })
  vim.keymap.set('n', '<leader>t', function() vscode.action('editor.action.goToTypeDefinition') end, { silent = true })
  vim.keymap.set('n', '<leader>u', function() vscode.action('editor.action.goToReferences') end, { silent = true })

  -- Multi cursor support
  vim.keymap.set('n', '<C-n>', function() vscode.action('editor.action.addSelectionToNextFindMatch') end, { silent = true })
  vim.keymap.set('x', '<C-n>', function() vscode.action('editor.action.addSelectionToNextFindMatch') end, { silent = true })

  -- Clear search highlighting
  vim.keymap.set('n', '<leader><space>', function() vscode.action('editor.action.clearFind') end, { silent = true })

  -- Save file
  vim.keymap.set('n', '<C-s>', function() vscode.action('workbench.action.files.save') end, { silent = true })
  vim.keymap.set('i', '<C-s>', function()
    vscode.with_insert(function()
      vscode.action('workbench.action.files.save')
    end)
  end, { silent = true })

  -- Buffer/tab navigation
  vim.keymap.set('n', '<leader>x', function() vscode.action('workbench.action.nextEditor') end, { silent = true })
  vim.keymap.set('n', '<leader>w', function() vscode.action('workbench.action.nextEditor') end, { silent = true })
  vim.keymap.set('n', '<leader>z', function() vscode.action('workbench.action.previousEditor') end, { silent = true })
  vim.keymap.set('n', '<leader>q', function() vscode.action('workbench.action.previousEditor') end, { silent = true })
  vim.keymap.set('n', '<leader>c', function() vscode.action('workbench.action.closeActiveEditor') end, { silent = true })

  -- Window navigation
  vim.keymap.set('n', '<C-h>', function() vscode.action('workbench.action.navigateLeft') end, { silent = true })
  vim.keymap.set('n', '<C-j>', function() vscode.action('workbench.action.navigateDown') end, { silent = true })
  vim.keymap.set('n', '<C-k>', function() vscode.action('workbench.action.navigateUp') end, { silent = true })
  vim.keymap.set('n', '<C-l>', function() vscode.action('workbench.action.navigateRight') end, { silent = true })

  -- Split windows
  vim.keymap.set('n', '<Leader>h', function() vscode.action('workbench.action.splitEditorDown') end, { silent = true })
  vim.keymap.set('n', '<Leader>v', function() vscode.action('workbench.action.splitEditorRight') end, { silent = true })
end
