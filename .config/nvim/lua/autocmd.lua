--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

local api = vim.api

local group_id = api.nvim_create_augroup('InitLua', {})
-- retrieve any external changes and refresh the buffer
api.nvim_create_autocmd('CursorHold', {
  group = group_id,
  pattern = '*',
  callback = function()
    if #vim.opt.buftype:get() == 0 then
      vim.cmd('checktime %')
    end
  end,
})
-- disable diagnostics in .env file
api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
  group = group_id,
  pattern = '.env',
  callback = function(arg)
    vim.diagnostic.disable(arg.buf)
  end,
})
-- hide column numbers when viewing man pages
api.nvim_create_autocmd('FileType', {
  group = group_id,
  pattern = 'man',
  command = 'set nonumber',
})
-- highlight the selection when yanking
api.nvim_create_autocmd('TextYankPost', {
  group = group_id,
  pattern = '*',
  callback = function()
    vim.highlight.on_yank()
  end,
})
