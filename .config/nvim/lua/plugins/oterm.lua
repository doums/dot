--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for oterm.nvim

local map = vim.keymap.set
local open = require('oterm').open

require('oterm').setup({
  terminal_hl = 'terminal',
  split_hl = 'terminal',
  border_hl = 'otermBorder',
  win_api = {
    border = { '┏', '━', '┓', '┃', '┛', '━', '┗', '┃' },
  },
})

map('n', '<M-t>', open)
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
map('n', '<F2>', function()
  open({
    name = 'gitui',
    layout = 'center',
    height = 0.8,
    width = 0.8,
    command = 'gitui',
  })
end)
map('n', '<F3>', function()
  open({
    name = 'tig',
    layout = 'center',
    height = 0.8,
    width = 0.7,
    command = 'tig',
  })
end)
