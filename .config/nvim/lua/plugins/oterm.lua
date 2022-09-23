--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for oterm.nvim

local map = vim.keymap.set
local open = require('oterm').open

require('oterm').setup({
  terminal_hl = 'terminal',
  split_hl = 'terminal',
})

map('n', '<M-t>', function()
  open()
end)
map('n', '<M-y>', function()
  open({ layout = 'vsplit' })
end)
map('n', '<M-n>', function()
  open({
    name = 'nnn',
    layout = 'center',
    height = 0.7,
    width = 0.6,
    command = 'nnn',
  })
end)
