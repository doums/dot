--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for nvim-tree.lua

local map = vim.keymap.set
local uv = vim.loop

map('n', '<Tab>', '<cmd>NvimTreeToggle<CR>')
map('n', '<S-Tab>', '<cmd>NvimTreeFindFile<CR>')

-- live grep using Telescope inside the current directory under
-- the cursor (or the parent directory of the current file)
local function grep_in(node)
  if not node then
    return
  end
  local path = node.absolute_path or uv.cwd()
  if node.type ~= 'directory' and node.parent then
    path = node.parent.absolute_path
  end
  require('telescope.builtin').live_grep({
    search_dirs = { path },
    prompt_title = string.format('Grep in [%s]', vim.fs.basename(path)),
  })
end

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
  git = {
    enable = true,
    ignore = false,
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
        { key = '<A-d>', action = 'toggle_file_info' },
        { key = 'g?', action = 'toggle_help' },
        { key = '<C-f>', action = '', action_cb = grep_in, mode = 'n' },
      },
    },
  },
})
