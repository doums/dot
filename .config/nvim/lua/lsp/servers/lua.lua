--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- sumneko_lua - Lua

local capabilities = require('lsp.common').capabilities

local runtime_path = vim.tbl_extend(
  'keep',
  vim.split(package.path, ';'),
  { '?/init.lua', '?.lua', 'lua/?.lua', 'lua/?/init.lua' }
)
require('lspconfig').sumneko_lua.setup({ -- Lua
  on_attach = function(client)
    -- use null-ls to handle formatting (stylua)
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
  end,
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = { version = 'LuaJIT', path = runtime_path },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { 'vim' },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file('', true),
      },
      telemetry = { enable = false },
    },
  },
})
