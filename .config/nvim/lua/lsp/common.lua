--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- server capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
require('lsp_spinner').init_capabilities(capabilities)
require('cmp_nvim_lsp').update_capabilities(capabilities)

local M = {
  capabilities = capabilities
}
return M
