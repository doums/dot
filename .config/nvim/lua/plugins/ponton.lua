--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for ponton.nvim

local winbar_bg = '#432717'
local line_bg = '#332A25'
local line_fg = '#734c36'
local ponton_cdt = require('ponton.condition')
local main_cdt = {
  ponton_cdt.filetype_not,
  { 'NvimTree', 'Trouble', 'TelescopePrompt' },
}
local cdts = {
  main_cdt,
}
require('ponton').setup({
  line = {
    'void',
    'mode',
    'buffer_changed',
    'read_only',
    'git_branch',
    'spacer',
    'lsp_spinner',
    'lsp_error',
    'lsp_warning',
    'lsp_information',
    'lsp_hint',
    'line',
    'sep',
    'column',
    'line_percent',
  },
  winbar = {
    'buffer_name',
    'buffer_changed_winbar',
  },
  top_line_exclude = { 'NvimTree', 'Trouble', 'TelescopePrompt' },
  segments = {
    mode = {
      map = {
        normal = { ' ' },
        insert = { '❱', { '#69ff00', line_bg, 'bold' } },
        replace = { '❰', { '#ff0050', line_bg, 'bold' } },
        visual = { '◆', { '#43A8ED', line_bg, 'bold' } },
        v_line = { '━', { '#43A8ED', line_bg, 'bold' } },
        v_block = { '■', { '#43A8ED', line_bg, 'bold' } },
        select = { '■', { '#3592C4', line_bg, 'bold' } },
        command = { '▼', { '#BDAE9D', line_bg, 'bold' } },
        shell_ex = { '▶', { '#93896C', line_bg, 'bold' } },
        terminal = { '❯', { '#049B0A', line_bg, 'bold' } },
        prompt = { '▼', { '#BDAE9D', line_bg, 'bold' } },
        inactive = { ' ' },
      },
      margin = { 1, 1 },
    },
    void = {
      length = '20%',
    },
    buffer_changed = {
      style = { '#a3f307', line_bg, 'bold' },
      value = '✶',
      padding = { nil, 1 },
      conditions = cdts,
    },
    buffer_name = {
      empty = "▫▫▫",
      style = {
        { '#BDAE9D', winbar_bg, 'bold' },
        { '#BDAE9D', '#2A190E', 'bold' },
      },
      padding = { 1, 1 },
      conditions = { main_cdt },
    },
    buffer_changed_winbar = {
      provider = 'buffer_changed',
      style = {
        { '#a3f307', winbar_bg, 'bold' },
        { '#a3f307', '#2A190E', 'bold' },
      },
      value = '✶',
      padding = { nil, 1 },
      conditions = cdts,
    },
    read_only = {
      style = { '#C75450', line_bg, 'bold' },
      value = '',
      padding = { nil, 1 },
      conditions = {
        ponton_cdt.is_read_only,
        ponton_cdt.is_normal_buf,
      },
    },
    spacer = {},
    sep = {
      style = { line_fg, line_bg },
      text = '⏽',
      conditions = cdts,
    },
    line_percent = {
      style = { line_fg, line_bg },
      padding = { nil, 1 },
      conditions = cdts,
    },
    line = {
      style = { line_fg, line_bg },
      padding = { 1 },
      conditions = cdts,
    },
    column = {
      style = { line_fg, line_bg },
      left_adjusted = true,
      padding = { nil, 1 },
      conditions = cdts,
    },
    git_branch = {
      style = { '#C5656B', line_bg },
      padding = { 1, 1 },
      prefix = ' ',
    },
    lsp_spinner = {
      style = { '#C5656B', line_bg },
      fn = require('lsp_spinner').status,
      padding = { nil, 2 },
      prefix = '󰣪 ',
      conditions = cdts,
    },
    lsp_error = {
      style = { '#FF0000', line_bg, 'bold' },
      padding = { nil, 1 },
      prefix = '✕',
      conditions = cdts,
    },
    lsp_warning = {
      style = { '#FFFF00', line_bg, 'bold' },
      padding = { nil, 1 },
      prefix = '▲',
      conditions = cdts,
    },
    lsp_information = {
      style = { '#FFFFCC', line_bg },
      padding = { nil, 1 },
      prefix = '╸',
      conditions = cdts,
    },
    lsp_hint = {
      style = { '#F49810', line_bg },
      padding = { nil, 1 },
      prefix = '•',
      conditions = cdts,
    },
  },
})
