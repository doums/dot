--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for Trouble

local map = vim.keymap.set

require('trouble').setup({
  height = 8,
  indent_lines = false,
  padding = false,
  action_keys = {
    open_split = { '<c-s>' }, -- open buffer in new split
    close_folds = { '<bs>' }, -- close all folds
  },
  signs = {
    error = '✕',
    warning = '▲',
    information = '╸',
    hint = '•',
    other = '╍',
  },
})
map(
  'n',
  '<A-S-q>',
  '<cmd>TroubleToggle workspace_diagnostics<cr>',
  { silent = true }
)
map(
  'n',
  '<A-q>',
  '<cmd>TroubleToggle document_diagnostics<cr>',
  { silent = true }
)
--[[ map('n', '<A-b>', '<cmd>Trouble lsp_definitions<cr>')
map('n', '<A-S-b>', '<cmd>Trouble lsp_type_definitions<cr>') ]]
map('n', '<A-u>', '<cmd>Trouble lsp_references<cr>', { silent = true })
