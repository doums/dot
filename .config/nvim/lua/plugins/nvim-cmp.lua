--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for nvim-cmp

local api = vim.api
local cmp = require('cmp')
local ls = require('luasnip')

local function has_word_before()
  local line, col = unpack(api.nvim_win_get_cursor(0))
  return col ~= 0
    and api
        .nvim_buf_get_lines(0, line - 1, line, true)[1]
        :sub(col, col)
        :match('%s')
      == nil
end

local tab_key = cmp.mapping(function(fallback)
  if cmp.visible() then
    cmp.select_next_item()
  elseif ls.expand_or_jumpable() then
    ls.expand_or_jump()
  elseif has_word_before() then
    cmp.complete()
  else
    fallback()
  end
end, {
  'i',
  -- fix cmdline autocplt, workaround
  -- see https://github.com/hrsh7th/nvim-cmp/issues/875
  c = cmp.config.disable,
  's',
})

local stab_key = cmp.mapping(function(fallback)
  if cmp.visible() then
    cmp.select_prev_item()
  elseif ls.jumpable(-1) then
    ls.jump(-1)
  else
    fallback()
  end
end, {
  'i',
  'c',
})

cmp.setup({
  mapping = {
    ['<tab>'] = tab_key,
    ['<S-tab>'] = stab_key,
    ['<Down>'] = cmp.mapping(
      cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
      { 'i' }
    ),
    ['<Up>'] = cmp.mapping(
      cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
      { 'i' }
    ),
    ['<M-p>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<M-o>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ['<cr>'] = cmp.mapping.confirm({ select = true }),
  },
  snippet = {
    expand = function(args)
      ls.lsp_expand(args.body)
    end,
  },
  completion = { completeopt = 'menu,menuone,noinsert' },
  sources = {
    { name = 'luasnip' },
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'path' },
    { name = 'buffer' },
  },
  window = {
    completion = { border = nil },
    documentation = { border = { '', '', '', ' ', '', '', '', ' ' } },
  },
  formatting = {
    format = function(entry, vim_item)
      vim_item.menu = ({
        buffer = '⌈buf⌋',
        nvim_lsp = '⌈lsp⌋',
        luasnip = '⌈snip⌋',
        nvim_lua = '⌈lua⌋',
        path = '⌈path⌋',
      })[entry.source.name]
      return vim_item
    end,
  },
})
-- command line `:`
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'nvim_lua' },
    { name = 'cmdline' },
  },
})
-- command line `/`
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' },
  },
})
