--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for nvim-tree.lua

local map = vim.keymap.set
local hl = require('utils').hl
local li = require('utils').li

map('n', '<Tab>', '<cmd>NvimTreeToggle<CR>')
map('n', '<S-Tab>', '<cmd>NvimTreeFindFile<CR>')

require('nvim-tree').setup({
  hijack_cursor = true,
  diagnostics = {
    enable = true,
    icons = {
      hint = '',
      info = '',
      warning = '',
      error = '✕',
    },
  },
  actions = {
    open_file = {
      window_picker = {
        exclude = {
          filetype = { 'Trouble', 'qf' },
          buftype = { 'nofile', 'terminal', 'help' },
        },
        chars = 'HLJKFQDS',
      },
    },
  },
  live_filter = {
    prefix = 'filter → ',
    always_show_folders = true,
  },
  renderer = {
    icons = {
      show = {
        git = false,
        folder = true,
        file = true,
        folder_arrow = false,
      },
      glyphs = {
        symlink = '󰌹',
        folder = {
          arrow_open = '▼',
          arrow_closed = '▶',
          default = '▶',
          open = '▼',
          empty = '▷',
          empty_open = '▽',
          symlink = '󰌹 ▶',
          symlink_open = '󰌹 ▼',
        },
      },
    },
    highlight_git = true,
  },
  view = {
    width = 40,
    mappings = {
      custom_only = true,
      list = {
        { key = { '<CR>', '<2-LeftMouse>' }, action = 'edit' },
        { key = { '<2-RightMouse>', '<C-]>' }, action = 'cd' },
        { key = '<C-v>', action = 'vsplit' },
        { key = '<C-s>', action = 'split' },
        { key = '<C-t>', action = 'tabnew' },
        { key = '<BS>', action = 'close_node' },
        { key = '<S-CR>', action = 'close_node' },
        { key = '<Tab>', action = 'preview' },
        { key = 'K', action = 'first_sibling' },
        { key = 'J', action = 'last_sibling' },
        { key = 'I', action = 'toggle_git_ignored' },
        { key = 'H', action = 'toggle_dotfiles' },
        { key = 'R', action = 'refresh' },
        { key = 'a', action = 'create' },
        { key = 'd', action = 'remove' },
        { key = 'r', action = 'rename' },
        { key = '<C-r>', action = 'full_rename' },
        { key = 'x', action = 'cut' },
        { key = 'c', action = 'copy' },
        { key = 'p', action = 'paste' },
        { key = 'y', action = 'copy_name' },
        { key = 'Y', action = 'copy_path' },
        { key = 'gy', action = 'copy_absolute_path' },
        { key = '-', action = 'dir_up' },
        { key = 'o', action = 'system_open' },
        { key = 'f', action = 'live_filter' },
        { key = 'F', action = 'clear_live_filter' },
        { key = 'q', action = 'close' },
        { key = 'g?', action = 'toggle_help' },
      },
    },
  },
})
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
