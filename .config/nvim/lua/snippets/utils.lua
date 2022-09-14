--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

local ls = require('luasnip')
local f = ls.function_node

local function filename()
  return f(function(_args, snip)
    -- remove file extension
    return string.gsub(snip.snippet.env.TM_FILENAME, '%..*$', '')
  end)
end

local function copy(index)
  return f(function(args)
    return args[1]
  end, { index })
end

local M = {
  filename = filename,
  copy = copy,
}
return M
