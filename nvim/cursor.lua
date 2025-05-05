-- Cursor-specific configuration
-- This file contains settings specifically for Cursor IDE integration

-- Ensure cursor integration
if vim.g.vscode then
  -- VSCode/Cursor integration
  print("Cursor IDE integration active")


  -- Define Cursor-specific commands
  -- These commands are specific to Cursor functionality

  -- Override key bindings with Cursor-specific commands
  vim.keymap.set('n', '<leader>e', "<cmd>call VSCodeNotify('workbench.action.quickOpen')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>f', "<cmd>call VSCodeNotify('workbench.action.findInFiles')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>p', "<cmd>call VSCodeNotify('editor.action.formatDocument')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>d', "<cmd>call VSCodeNotify('editor.action.revealDefinition')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>g', "<cmd>call VSCodeNotify('editor.action.showHover')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>r', "<cmd>call VSCodeNotify('editor.action.rename')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>s', "<cmd>call VSCodeNotify('editor.action.sourceAction')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>a', "<cmd>call VSCodeNotify('editor.action.marker.next')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>A', "<cmd>call VSCodeNotify('editor.action.marker.prev')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>i', "<cmd>call VSCodeNotify('editor.action.goToImplementation')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>t', "<cmd>call VSCodeNotify('editor.action.goToTypeDefinition')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>u', "<cmd>call VSCodeNotify('editor.action.goToReferences')<CR>", { silent = true })

  -- Multi cursor support
  vim.keymap.set('n', '<C-n>', "<cmd>call VSCodeNotify('editor.action.addSelectionToNextFindMatch')<CR>", { silent = true })
  vim.keymap.set('x', '<C-n>', "<cmd>call VSCodeNotify('editor.action.addSelectionToNextFindMatch')<CR>", { silent = true })

  -- Clear search highlighting
  vim.keymap.set('n', '<leader><space>', "<cmd>call VSCodeNotify('editor.action.clearFind')<CR>", { silent = true })

  -- Save file
  vim.keymap.set('n', '<C-s>', "<cmd>call VSCodeNotify('workbench.action.files.save')<CR>", { silent = true })
  vim.keymap.set('i', '<C-s>', "<Esc><cmd>call VSCodeNotify('workbench.action.files.save')<CR>", { silent = true })

  -- Buffer/tab navigation
  vim.keymap.set('n', '<leader>x', "<cmd>call VSCodeNotify('workbench.action.nextEditor')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>w', "<cmd>call VSCodeNotify('workbench.action.nextEditor')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>z', "<cmd>call VSCodeNotify('workbench.action.previousEditor')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>q', "<cmd>call VSCodeNotify('workbench.action.previousEditor')<CR>", { silent = true })
  vim.keymap.set('n', '<leader>c', "<cmd>call VSCodeNotify('workbench.action.closeActiveEditor')<CR>", { silent = true })

  -- Window navigation
  vim.keymap.set('n', '<C-h>', "<cmd>call VSCodeNotify('workbench.action.navigateLeft')<CR>", { silent = true })
  vim.keymap.set('n', '<C-j>', "<cmd>call VSCodeNotify('workbench.action.navigateDown')<CR>", { silent = true })
  vim.keymap.set('n', '<C-k>', "<cmd>call VSCodeNotify('workbench.action.navigateUp')<CR>", { silent = true })
  vim.keymap.set('n', '<C-l>', "<cmd>call VSCodeNotify('workbench.action.navigateRight')<CR>", { silent = true })

  -- Split windows
  vim.keymap.set('n', '<Leader>h', "<cmd>call VSCodeNotify('workbench.action.splitEditorDown')<CR>", { silent = true })
  vim.keymap.set('n', '<Leader>v', "<cmd>call VSCodeNotify('workbench.action.splitEditorRight')<CR>", { silent = true })
end
