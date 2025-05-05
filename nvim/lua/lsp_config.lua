-- LSP Configuration
-- Provides functionality similar to CoC in the original Vim config

local M = {}

function M.setup()
  local lspconfig_status_ok, lspconfig = pcall(require, "lspconfig")
  if not lspconfig_status_ok then
    return
  end

  -- Configure diagnostic display
  vim.diagnostic.config({
    virtual_text = {
      prefix = '●',
      severity = nil,
      source = "if_many",
    },
    float = {
      source = "always",
      border = "rounded",
    },
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
  })

  -- Define signs that match the original config
  local signs = {
    { name = "DiagnosticSignError", text = "✘" },
    { name = "DiagnosticSignWarn", text = "➜" },
    { name = "DiagnosticSignHint", text = "✚" },
    { name = "DiagnosticSignInfo", text = "±" },
  }

  for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
  end

  -- Global mappings.
  -- See `:help vim.diagnostic.*` for documentation on any of the below functions
  vim.keymap.set('n', '<leader>a', vim.diagnostic.goto_next, { desc = "Go to next diagnostic" })
  vim.keymap.set('n', '<leader>A', vim.diagnostic.goto_prev, { desc = "Go to previous diagnostic" })
  vim.keymap.set('n', '<leader>G', vim.diagnostic.open_float, { desc = "Show diagnostic details" })
  vim.keymap.set('n', '<leader>l', vim.diagnostic.setloclist, { desc = "List diagnostics" })

  -- Use LspAttach autocommand to only map the following keys
  -- after the language server attaches to the current buffer
  vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
      local client = vim.lsp.get_client_by_id(ev.data.client_id)
      local bufnr = ev.buf

      -- Enable completion triggered by <c-x><c-o>
      vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'

      -- Buffer local mappings.
      -- See `:help vim.lsp.*` for documentation on any of the below functions
      local opts = { buffer = bufnr }
      vim.keymap.set('n', '<leader>d', vim.lsp.buf.definition, { buffer = bufnr, desc = "Go to definition" })
      vim.keymap.set('n', '<leader>g', vim.lsp.buf.hover, { buffer = bufnr, desc = "Show hover information" })
      vim.keymap.set('n', '<leader>i', vim.lsp.buf.implementation, { buffer = bufnr, desc = "Go to implementation" })
      vim.keymap.set('n', '<leader>t', vim.lsp.buf.type_definition, { buffer = bufnr, desc = "Go to type definition" })
      vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, { buffer = bufnr, desc = "Rename symbol" })
      vim.keymap.set('n', '<leader>R', function() vim.lsp.buf.references(nil, { includeDeclaration = false }) end, { buffer = bufnr, desc = "Find references" })
      vim.keymap.set('n', '<leader>u', vim.lsp.buf.references, { buffer = bufnr, desc = "Find usages" })
      vim.keymap.set('n', '<leader>s', vim.lsp.buf.code_action, { buffer = bufnr, desc = "Code actions" })
      vim.keymap.set('x', '<leader>s', vim.lsp.buf.code_action, { buffer = bufnr, desc = "Code actions (selection)" })
      vim.keymap.set('n', '<leader>S', function()
        vim.lsp.buf.code_action({ context = { only = { "quickfix", "refactor", "source" } } })
      end, { buffer = bufnr, desc = "Fix current issue" })

      -- Handle formatting
      if client and client.server_capabilities.documentFormattingProvider then
        vim.keymap.set('n', '<leader>p', function() vim.lsp.buf.format({ async = true }) end, { buffer = bufnr, desc = "Format document" })
      end

      -- Organize imports (if supported by language server)
      vim.keymap.set('n', '<leader>O', function()
        if client.name == "tsserver" then
          vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
        elseif client.name == "eslint" then
          vim.cmd("EslintFixAll")
        end
      end, { buffer = bufnr, desc = "Organize imports" })
    end,
  })

  -- Setup language servers
  -- JavaScript / TypeScript
  if vim.fn.executable('typescript-language-server') == 1 then
    lspconfig.tsserver.setup({
      settings = {
        typescript = {
          inlayHints = {
            includeInlayParameterNameHints = "all",
            includeInlayParameterNameHintsWhenArgumentMatchesName = false,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayVariableTypeHints = true,
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayEnumMemberValueHints = true,
          },
        },
        javascript = {
          inlayHints = {
            includeInlayParameterNameHints = "all",
            includeInlayParameterNameHintsWhenArgumentMatchesName = false,
            includeInlayFunctionParameterTypeHints = true,
            includeInlayVariableTypeHints = true,
            includeInlayPropertyDeclarationTypeHints = true,
            includeInlayFunctionLikeReturnTypeHints = true,
            includeInlayEnumMemberValueHints = true,
          },
        },
      },
    })
  end

  -- ESLint
  if vim.fn.executable('eslint') == 1 then
    lspconfig.eslint.setup({})
  end

  -- JSON
  if vim.fn.executable('vscode-json-language-server') == 1 then
    lspconfig.jsonls.setup({
      settings = {
        json = {
          schemas = require('schemastore').json.schemas(),
          validate = { enable = true },
        },
      },
    })
  end

  -- YAML
  if vim.fn.executable('yaml-language-server') == 1 then
    lspconfig.yamlls.setup({
      settings = {
        yaml = {
          schemaStore = {
            enable = true,
            url = "https://www.schemastore.org/api/json/catalog.json",
          },
        },
      },
    })
  end

  -- Docker
  if vim.fn.executable('docker-langserver') == 1 then
    lspconfig.dockerls.setup({})
  end

  -- Other language servers can be configured here
end

return M
