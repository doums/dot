--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for leap.nvim

require('leap').set_default_keymaps()

-- stylua: ignore start
require('leap').setup({
  safe_labels = { 's', 'f', 'n', 'u', 't', 'b', 'g', 'F',
    'L', 'N', 'H', 'G', 'M', 'U', 'T', 'Z' },
  labels = { 's', 'f', 'n', 'g', 'h', 'v', 'b', 'w', 'y', 'd',
    'q', 'z', 'c', 'x', 't', 'u', 'r', 'i', 'a', 'o', 'e' },
})
