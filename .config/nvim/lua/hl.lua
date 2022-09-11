--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

local M = {}

local hl = require('utils').hl
local li = require('utils').li

-- after loading a colorscheme, re-generate hl groups
vim.api.nvim_create_autocmd('ColorScheme', {
  pattern = '*',
  callback = function()
    M.hl()
    print('hl groups have been generated ✓')
  end,
})

function M.hl()
  -- highlight group for guicursor
  hl('Caret', '#2A211C', '#889AFF', 'bold')
  hl('WinSeparator', '#332a25', '#332a25')

  -- floaterm.nvim
  hl('otermSplit', '#2A190E', '#211a16')

  -- lightspeed.nvim
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

  -- nvim-cmp
  li('CmpItemAbbr', 'Pmenu')
  li('CmpItemAbbrDeprecated', 'Pmenu')
  hl('CmpItemAbbrMatch', '#CA7E03', '#432717', 'bold')
  hl('CmpItemAbbrMatchFuzzy', '#CA7E03', '#432717', 'bold')
  li('CmpItemKind', 'Pmenu')
  li('CmpItemMenu', 'Pmenu')

  -- nvim-tree.lua
  li('NvimTreeRootFolder', 'Comment')
  li('NvimTreeExecFile', 'Todo')
  li('NvimTreeSpecialFile', 'Function')
  li('NvimTreeFolderIcon', 'Constant')
  li('NvimTreeImageFile', 'Normal')
  li('NvimTreeGitIgnored', 'Debug')
  hl('NvimTreeGitNew', '#42905b', nil, 'italic')
  hl('NvimTreeGitStaged', '#39c064', nil, 'italic')
  hl('NvimTreeGitRenamed', '#507eae', nil, 'italic')
  hl('NvimTreeGitDeleted', '#bd5b5b', nil, 'italic')
  li('NvimTreeGitDirty', 'NvimTreeGitDeleted')
  hl('NvimTreeWindowPicker', '#BDAE9D', '#2A190E', 'bold')
  hl('NvimTreeLspDiagnosticsError', '#FF0000', nil, 'bold')
  li('NvimTreeWinSeparator', 'WinSeparator')
  li('NvimTreeLiveFilterPrefix', 'CurSearch')
  li('NvimTreeLiveFilterValue', 'Search')

  -- ponton.nvim
  hl('WinBar', '#2A211C', '#2A211C')
  hl('WinBarNC', '#2A211C', '#2A211C')
  hl('StatusLine', '#2A211C', '#332A25')

  -- suit.nvim
  hl('suitPrompt', '#C7C7FF', '#1d1916', { 'bold', 'italic' })
  hl('suitInput', '#BDAE9D', '#1d1916')
  hl('suitSelectedItem', nil, '#3b2f27')

  -- telescope.nvim
  li('TelescopeNormal', 'Fg')
  hl('TelescopeSelection', nil, '#3b2f27')

  -- Trouble
  li('TroubleCount', 'Number')
  li('TroubleText', 'Fg')
  li('TroubleLocation', 'NonText')
  li('TroubleFoldIcon', 'Constant')

  -- lsp_signature.nvim
  hl('signatureHint', '#CA7E03', nil, 'italic')
end

return M
