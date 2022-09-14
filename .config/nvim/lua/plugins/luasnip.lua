--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for LuaSnip

require('luasnip.loaders.from_lua').load({
  paths = './lua/snippets/ft/',
})
require('luasnip').filetype_extend('typescriptreact', { 'typescript' })
