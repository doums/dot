--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- null-ls.nvim config

local capabilities = require('lsp.common').capabilities

local null_ls = require('null-ls')
local cspell_src = null_ls.builtins.diagnostics.cspell
cspell_src.disabled_filetypes = { 'NvimTree', 'c', 'cpp' }
null_ls.setup({
  sources = {
    null_ls.builtins.diagnostics.eslint,
    null_ls.builtins.diagnostics.shellcheck,
    cspell_src,
    null_ls.builtins.formatting.prettier,
    null_ls.builtins.formatting.stylua,
    null_ls.builtins.formatting.prismaFmt,
  },
  fallback_severity = vim.diagnostic.severity.HINT,
  capabilities = capabilities,
})
