--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- rust-analyzer - Rust

local capabilities = require('lsp.common').capabilities

require('rust-tools').setup({
  tools = {
    hover_with_actions = false,
    inlay_hints = {
      autoSetHints = false,
      parameter_hints_prefix = '← ',
      other_hints_prefix = '→ ',
      highlight = 'codeHint',
    },
    hover_actions = { border = 'none' },
  },
  server = { -- rust-analyzer server options
    capabilities = capabilities,
    settings = {
      ['rust-analyzer'] = { checkOnSave = { command = 'clippy' } },
    },
  },
})
