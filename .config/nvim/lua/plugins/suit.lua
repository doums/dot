--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for suit.nvim

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
