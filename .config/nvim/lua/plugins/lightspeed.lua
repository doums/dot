--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for lightspeed.nvim

local hl = require('utils').hl

-- stylua: ignore start
require('lightspeed').setup({
  exit_after_idle_msecs = { labeled = 1500, unlabeled = 1500 },
  safe_labels = { 's', 'f', 'n', 'u', 't', 'b', 'g', 'F',
    'L', 'N', 'H', 'G', 'M', 'U', 'T', 'Z' },
  labels = { 's', 'f', 'n', 'g', 'h', 'v', 'b', 'w', 'y', 'd',
    'q', 'z', 'c', 'x', 't', 'u', 'r', 'i', 'a', 'o', 'e' },
  repeat_ft_with_target_char = true
})
-- stylua: ignore end
hl('LightspeedCursor', '#212121', '#ebff00', 'bold')
hl('LightspeedLabel', '#f49810', nil, { 'bold', 'underline' })
hl('LightspeedLabelOverlapped', '#f49810', nil, 'underline')
hl('LightspeedShortcut', '#212121', '#f49810', { 'bold', 'underline' })
hl('LightspeedOneCharMatch', '#212121', '#f49810', 'bold')
hl('LightspeedGreyWash', '#80807f')
hl('LightspeedUnlabeledMatch', '#ddddff', nil, 'bold')
hl('LightspeedPendingOpArea', '#212121', '#f49810')
hl('LightspeedLabelDistant', '#aa4e00', nil, { 'bold', 'underline' })
hl('LightspeedLabelDistantOverlapped', '#aa4e00', nil, 'underline')
hl('LightspeedMaskedChar', '#906526')
