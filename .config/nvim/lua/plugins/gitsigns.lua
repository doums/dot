--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for gitsigns.nvim

local map = vim.keymap.set
local gs = require('gitsigns')
local git_menu = require('plugins.git_menu')

gs.setup({
  signs = {
    add = { hl = 'GitAddSign', text = '┃' },
    change = { hl = 'GitChangeSign', text = '┃' },
    delete = { hl = 'GitDeleteSign', text = '▶' },
    topdelete = { hl = 'GitDeleteSign', text = '▶' },
    changedelete = { hl = 'GitChangeDeleteSign', text = '┃' },
  },
  numhl = false,
  linehl = false,
  current_line_blame_formatter = '<author>, <author_time:%d-%m-%Y> - <summary>',
  on_attach = function(bufnr)
    local opts = { buffer = bufnr }
    map('n', '<A-g>', git_menu, opts)
    map('n', '<leader>n', gs.next_hunk, opts)
    map('n', '<leader>N', gs.prev_hunk, opts)
  end,
  preview_config = { border = { '', '', '', ' ', '', '', '', ' ' } },
})
