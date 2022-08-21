--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Custom menu for git operations

local gs = require('gitsigns')

local git_static_actions = {
  ['Buffer commits'] = require('telescope.builtin').git_bcommits,
  ['Diff'] = gs.diffthis,
  ['Diff ~'] = function()
    gs.diffthis('~')
  end,
  ['Changelist'] = function()
    gs.setloclist(0, 'all')
  end,
  ['Refresh'] = gs.refresh,
  ['Stage buffer'] = gs.stage_buffer,
  ['Stash'] = require('telescope.builtin').git_stash,
  ['Rollback'] = function()
    vim.ui.select({ 'OK', 'Cancel' }, {
      prompt = 'Rollback:',
    }, function(choice)
      if choice == 'OK' then
        gs.reset_buffer()
      end
    end)
  end,
}

local function git_menu()
  local git_actions =
    vim.tbl_extend('keep', gs.get_actions(), git_static_actions)
  local items = vim.tbl_keys(git_actions)
  table.sort(items)
  vim.ui.select(items, {
    prompt = 'git:',
  }, function(choice)
    if choice then
      git_actions[choice]()
    end
  end)
end

return git_menu
