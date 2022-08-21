--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for suit.nvim

local hl = require('utils').hl

hl('suitPrompt', '#C7C7FF', '#1d1916', { 'bold', 'italic' })
hl('suitInput', '#BDAE9D', '#1d1916')
hl('suitSelectedItem', nil, '#3b2f27')
require('suit').setup({
  input = {
    default_prompt = '↓',
    border = 'vgap',
    hl_prompt = 'suitPrompt',
    hl_input = 'suitInput',
    hl_border = 'suitInput',
  },
  select = {
    default_prompt = '≡',
    border = 'vgap',
    hl_prompt = 'suitPrompt',
    hl_select = 'suitInput',
    hl_border = 'suitInput',
    hl_selected_item = 'suitSelectedItem',
  },
})
