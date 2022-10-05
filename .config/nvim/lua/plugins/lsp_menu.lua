--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Custom menu for LSP operations

local lsp = vim.lsp

local lsp_actions = {
  ['Goto definition'] = lsp.buf.definition,
  ['Goto declaration'] = lsp.buf.declaration,
  ['Goto type definition'] = lsp.buf.type_definition,
  ['Find usages'] = lsp.buf.references,
  ['Refactor'] = lsp.buf.rename,
  ['Code action'] = lsp.buf.code_action,
  ['Signature help'] = lsp.buf.signature_help,
  ['Find implementations'] = lsp.buf.implementation,
  ['Format'] = lsp.buf.format,
}
local lsp_range_actions = {
  ['Code action'] = lsp.buf.code_action,
  ['Format'] = lsp.buf.format,
}

local function lsp_menu()
  local mode = vim.api.nvim_get_mode().mode
  local is_visual = mode == 'v' or mode == 'V' or mode == '\22'
  local actions = lsp_actions
  if is_visual then
    actions = lsp_range_actions
  end
  local lsp_items = vim.tbl_keys(actions)
  table.sort(lsp_items)
  vim.ui.select(lsp_items, {
    prompt = 'LSP:',
  }, function(choice)
    if choice then
      actions[choice]()
    end
  end)
end

return lsp_menu
