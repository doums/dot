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
    vim.notify(
      'hl groups have been generated ✓',
      vim.log.levels.INFO,
      { title = 'nvim-config' }
    )
  end,
})

function M.hl()
  -- highlight group for guicursor
  hl('Caret', '#2A211C', '#889AFF', 'bold')
  hl('WinSeparator', '#332a25', '#332a25')

  -- oterm.nvim
  hl('terminal', '#FFFFFF', '#212121')
  hl('otermBorder', '#2A190E', '#212121')

  -- leap.nvim
  hl('LeapMatch', '#aa4e00', nil, { 'underline', 'nocombine' })
  hl('LeapLabelPrimary', '#212121', '#f49810', 'nocombine')
  hl('LeapLabelSecondary', '#212121', '#8c5845', 'nocombine')
  hl('LeapLabelSelected', '#ddddff', nil, { 'bold', 'nocombine' })
  -- hl('LeapBackdrop', '#80807f', nil, 'nocombine')
  -- leap use Cursor hl group, customize it
  hl('Cursor', '#212121', '#df4a00', { 'nocombine', 'bold' })

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
  hl('StatusLine', '#734c36', '#332A25')

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
  hl('codeHint', '#CA7E03', nil, 'italic')
end

return M
