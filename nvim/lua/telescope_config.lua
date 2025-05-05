-- Telescope configuration
-- Provides functionality similar to FZF in the original Vim config

local M = {}

function M.setup()
  local status_ok, telescope = pcall(require, "telescope")
  if not status_ok then
    return
  end

  local actions = require "telescope.actions"

  telescope.setup {
    defaults = {
      prompt_prefix = " ",
      selection_caret = " ",
      path_display = { "truncate" },
      file_ignore_patterns = {
        "node_modules",
        ".git/",
        ".cache",
        "**/.stack-work",
        "**/.stack/",
        "**/dist",
        "**/build",
        "**/output",
        "**/.yarn",
        "*.so",
        "*.swp",
        "*.zip",
        "*.pyc",
        "*.db",
        "*.sqlite",
        "package-lock.json",
        "stack.yaml.lock",
        "yarn.lock",
        "*.o",
        "*.obj",
        ".git",
        "*.rbc",
        "__pycache__"
      },

      mappings = {
        i = {
          ["<C-j>"] = actions.move_selection_next,
          ["<C-k>"] = actions.move_selection_previous,
          ["<C-c>"] = actions.close,
          ["<Down>"] = actions.move_selection_next,
          ["<Up>"] = actions.move_selection_previous,
          ["<CR>"] = actions.select_default,
          ["<C-x>"] = actions.select_horizontal,
          ["<C-v>"] = actions.select_vertical,
        },
        n = {
          ["<esc>"] = actions.close,
          ["j"] = actions.move_selection_next,
          ["k"] = actions.move_selection_previous,
          ["H"] = actions.move_to_top,
          ["M"] = actions.move_to_middle,
          ["L"] = actions.move_to_bottom,
          ["<CR>"] = actions.select_default,
          ["<C-x>"] = actions.select_horizontal,
          ["<C-v>"] = actions.select_vertical,
        },
      },
    },
    pickers = {
      find_files = {
        hidden = true,
        find_command = { "rg", "--files", "--hidden", "--follow", "--no-require-git", "--glob", "!.git/*" },
      },
      live_grep = {
        additional_args = function()
          return { "--hidden" }
        end
      },
    },
    extensions = {
      -- Extensions can be configured here
    }
  }

  -- Load extensions if they are installed
  pcall(telescope.load_extension, "fzf")

  -- Key mappings
  local builtin = require('telescope.builtin')
  vim.keymap.set('n', '<leader>e', builtin.find_files, { desc = 'Find files' })
  vim.keymap.set('n', '<leader>E', function()
    local git_dir = vim.fn.system('git rev-parse --show-toplevel'):gsub('\n', '')
    if vim.v.shell_error == 0 then
      vim.fn.chdir(git_dir)
      builtin.find_files()
    else
      builtin.find_files()
    end
  end, { desc = 'Find files from git root' })

  vim.keymap.set('n', '<leader>f', builtin.live_grep, { desc = 'Search in files' })
  vim.keymap.set('n', '<leader>F', function()
    local git_dir = vim.fn.system('git rev-parse --show-toplevel'):gsub('\n', '')
    if vim.v.shell_error == 0 then
      vim.fn.chdir(git_dir)
      builtin.live_grep()
    else
      builtin.live_grep()
    end
  end, { desc = 'Search in files from git root' })

  vim.keymap.set('n', '<leader>b', builtin.buffers, { desc = 'Find buffers' })
  vim.keymap.set('n', '<leader>y', builtin.command_history, { desc = 'Command history' })
end

return M
