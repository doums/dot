--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- tsserver - TypeScript

local capabilities = require('lsp.common').capabilities

require('lspconfig').tsserver.setup({ -- TypeScript
  on_attach = function(client)
    -- use null-ls to handle formatting (Prettier)
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
  end,
  capabilities = capabilities,
})
