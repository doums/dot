--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Snippets for Lua

local ls = require('luasnip')
local s = ls.snippet
local fmt = require('luasnip.extras.fmt').fmt
local fmta = require('luasnip.extras.fmt').fmta
local i = ls.insert_node

local M = {
  s(
    {
      trig = 'mpl',
      name = 'MPL Header',
      dscr = 'Mozilla Public License v2.O Header',
    },
    fmt(
      [=[
  --[[ This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

  ]=],
      {}
    )
  ),
  s('prt', fmt([[print({})]], i(0))),
  s('prti', fmt([[print(vim.inspect({}))]], i(0))),
  s('fmt', fmt([[string.format('%s', {})]], i(0))),
  s('re', fmt([[local {} = require('{}')]], { i(0), i(1) })),
  s(
    { trig = 'lfn', name = 'Local function declaration' },
    fmta(
      [[
    local function <>(<>)
      <>
    end
		]],
      {
        i(1),
        i(2),
        i(0),
      },
      { dedent = true }
    )
  ),
  s(
    { trig = 'fn', name = 'Function declaration' },
    fmta(
      [[
    function <>(<>)
      <>
    end
		]],
      {
        i(1),
        i(2),
        i(0),
      },
      { dedent = true }
    )
  ),
}

return M
