--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for LSP

local lsp = vim.lsp
local api = vim.api
local map = vim.keymap.set
local fn = vim.fn

-- Server configs
require('lsp.servers.lua')
require('lsp.servers.c')
require('lsp.servers.rust')
require('lsp.servers.typescript')
require('lsp.servers.null-ls')

local lsp_menu = require('plugins.lsp_menu')
local lsp_spinner = require('lsp_spinner')
local lsp_signature = require('lsp_signature')

lsp_spinner.setup({
  spinner = { '▪', '■', '□', '▫' },
  interval = 80,
})

fn.sign_define(
  'DiagnosticSignError',
  { text = '✗', texthl = 'DiagnosticSignError' }
)
fn.sign_define(
  'DiagnosticSignWarn',
  { text = '▲', texthl = 'DiagnosticSignWarn' }
)
fn.sign_define(
  'DiagnosticSignInfo',
  { text = '╸', texthl = 'DiagnosticSignInfo' }
)
fn.sign_define(
  'DiagnosticSignHint',
  { text = '•', texthl = 'DiagnosticSignHint' }
)

local signature_help_cfg = {
  bind = true,
  doc_lines = 2,
  floating_window = true,
  hint_enable = true,
  hint_prefix = '→ ',
  hint_scheme = 'codeHint',
  hi_parameter = 'Search',
  max_height = 4,
  max_width = 80,
  handler_opts = { border = 'none' },
  padding = ' ',
  toggle_key = '<C-q>',
}

local function format_diagnostic(diagnostic)
  return string.format('%s (%s)', diagnostic.message, diagnostic.source)
end

local function prefix_diagnostic(diagnostic)
  local severity_map = {
    [vim.diagnostic.severity.ERROR] = { '╸ ', 'DiagnosticSignError' },
    [vim.diagnostic.severity.WARN] = { '╸ ', 'WarningSign' },
    [vim.diagnostic.severity.INFO] = { '╸ ', 'InformationSign' },
    [vim.diagnostic.severity.HINT] = { '╸ ', 'HintSign' },
  }
  return unpack(severity_map[diagnostic.severity])
end

-- vim.diagnostic config
vim.diagnostic.config({
  virtual_text = false,
  severity_sort = true,
  float = {
    header = false,
    format = format_diagnostic,
    prefix = prefix_diagnostic,
  },
})

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local client = lsp.get_client_by_id(args.data.client_id)
    local bufopt = { buffer = args.buf }
    -- keybinds
    map({ 'n', 'v' }, '<A-CR>', lsp_menu, bufopt)
    map('n', '<A-b>', lsp.buf.definition, bufopt)
    map(
      'n',
      '<A-a>',
      '<cmd>lua vim.diagnostic.goto_prev({float=false})<CR>',
      bufopt
    )
    map(
      'n',
      '<A-z>',
      '<cmd>lua vim.diagnostic.goto_next({float=false})<CR>',
      bufopt
    )
    map('n', '<A-d>', lsp.buf.hover, bufopt)
    map('n', '<A-i>', function()
      return require('rust-tools.inlay_hints').toggle_inlay_hints
    end, bufopt)
    map('n', '<A-r>', lsp.buf.rename, bufopt)
    map({ 'n', 'v' }, '<A-e>', lsp.buf.format, bufopt)
    -- open a floating window with the diagnostics from the current cursor position
    api.nvim_create_autocmd('CursorHold', {
      callback = function()
        vim.diagnostic.open_float({ focusable = false, scope = 'cursor' })
      end,
      buffer = args.buf,
    })
    -- highlight the symbol under the cursor
    if client.server_capabilities.documentHighlightProvider then
      api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
        callback = function()
          lsp.buf.document_highlight()
        end,
        buffer = args.buf,
      })
      api.nvim_create_autocmd('CursorMoved', {
        callback = function()
          lsp.buf.clear_references()
        end,
        buffer = args.buf,
      })
    end
    lsp_spinner.on_attach(client, args.buf)
    lsp_signature.on_attach(signature_help_cfg, args.buf)
  end,
})
