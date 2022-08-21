--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for floaterm.nvim

local map = vim.keymap.set

require('floaterm').setup({
  layout = 'bottom',
  width = 1,
  height = 0.4,
  bg_color = '#211a16',
})

require('oterm').setup({
  bg_color = '#211a16',
  split_hl = 'otermSplit',
})

map('n', '<M-t>', [[<cmd>lua require'oterm'.open()<cr>]])
map('n', '<M-y>', [[<cmd>lua require'oterm'.open({layout='vsplit'})<cr>]])
map(
  'n',
  '<M-n>',
  [[<cmd>lua require'floaterm'.open({name='nnn',layout='center',height=0.7,width=0.6,command='nnn'})<cr>]]
)
