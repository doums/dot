--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for nvim-treesitter

require('nvim-treesitter.configs').setup({
  ensure_installed = {
    'c',
    'cpp',
    'rust',
    'yaml',
    'bash',
    'typescript',
    'javascript',
    'html',
    'css',
    'lua',
    'comment',
    'markdown',
    'jsdoc',
    'tsx',
    'toml',
    'json',
    'graphql',
    'jsonc',
  },
  highlight = { enable = true, custom_captures = { todo = 'Todo' } },
  indent = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'tn',
      node_incremental = '<A-l>',
      scope_incremental = '<A-j>',
      node_decremental = '<A-h>',
    },
  },
  autotag = {
    enable = true,
    filetypes = { 'typescriptreact', 'tsx', 'markdown' },
  },
})
