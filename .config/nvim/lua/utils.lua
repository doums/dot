--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Helper functions

function _G.dump(...)
  local objects = vim.tbl_map(vim.inspect, { ... })
  print(unpack(objects))
end

local function hl(name, fg, bg, style, sp)
  local hl_map = { fg = fg, bg = bg, sp = sp }
  if type(style) == 'string' then
    hl_map[style] = 1
  elseif type(style) == 'table' then
    for _, v in ipairs(style) do
      hl_map[v] = 1
    end
  end
  vim.api.nvim_set_hl(0, name, hl_map)
end

local function li(target, source)
  vim.cmd(string.format('hi! link %s %s', target, source))
end

local M = {
  hl = hl,
  li = li,
}
return M
