--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Snippets for Lua

local ls = require('luasnip')
local s = ls.snippet
local fmt = require('luasnip.extras.fmt').fmt
local i = ls.insert_node

local M = {
  s('prt', fmt([[print({})]], i(1))),
  s('prti', fmt([[print(vim.inspect({}))]], i(1))),
  s('fmt', fmt([[string.format('%s', {})]], i(1))),
}

return M
