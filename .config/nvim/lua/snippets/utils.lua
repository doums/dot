--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

local ls = require('luasnip')
local f = ls.function_node

-- helper to get the current filename
local function filename()
  return f(function(_args, snip)
    -- remove file extension
    return snip.snippet.env.TM_FILENAME:gsub('%..*$', '')
  end)
end

-- helper to copy the value of the given node
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
