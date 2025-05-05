-- Multi-cursor configuration
-- Configures vim-visual-multi to match original configuration

local M = {}

function M.setup()
  -- Set up vim-visual-multi
  vim.g.VM_maps = {
    ["Select All"] = '<leader><C-n>',
    ["Visual All"] = '<leader><C-n>',
  }

  -- Legacy mappings from the original vim config
  vim.g.VM_default_mappings = 0

  -- Define custom mappings
  vim.keymap.set('n', '<C-n>', '<Plug>(VM-Find-Under)', { desc = 'Multi cursor - add cursor at word' })
  vim.keymap.set('n', '<C-Up>', '<Plug>(VM-Add-Cursor-Up)', { desc = 'Multi cursor - add cursor up' })
  vim.keymap.set('n', '<C-Down>', '<Plug>(VM-Add-Cursor-Down)', { desc = 'Multi cursor - add cursor down' })

  -- Theme settings to better match the Dracula theme
  vim.g.VM_theme = 'codedark'

  -- Customize the highlight groups for better visibility
  vim.cmd([[
    augroup multi_cursor_highlights
      autocmd!
      autocmd ColorScheme * hi! link VM_Extend Cursor
      autocmd ColorScheme * hi! link VM_Cursor DiffAdd
      autocmd ColorScheme * hi! link VM_Insert DiffChange
      autocmd ColorScheme * hi! link VM_Mono IncSearch
    augroup END
  ]])
end

return M
