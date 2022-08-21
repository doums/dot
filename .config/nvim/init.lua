--[[ External dependencies

for LSP
  * clangd, language server for C/C++ (Arch Linux package clangd)
  * TypeScript Language Server (npm package - typescript-language-server)
  * rust-analyzer (https://rust-analyzer.github.io/manual.html#rustup)
  * shellcheck, shell script static analysis tool (AUR shellcheck-bin)
  * StyLua, Lua code formatter (AUR stylua)
  * lua-language-server, must be installed in /opt/lua-language-server
  * ESLint and Prettier (npm packages - eslint prettier)
  * Prisma LSP (npm package - @prisma/language-server)
  * LTEX LS https://github.com/valentjn/ltex-ls/releases
  * cspell (npm package)

others: git, ripgrep, fzf, node, npm
-- ]]

-- ALIASES -------------------------------------------------------
local fn = vim.fn
local cmd = vim.cmd
local g = vim.g
local opt = vim.opt
local map = vim.keymap.set

-- PLUGINS -------------------------------------------------------
-- auto install paq-nvim
local install_path = fn.stdpath('data') .. '/site/pack/paqs/opt/paq-nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  cmd(
    '!git clone --depth=1 https://github.com/savq/paq-nvim.git ' .. install_path
  )
end

cmd('packadd paq-nvim') -- Load package

-- update treesitter parsers
local function update_ts_parsers()
  cmd('TSUpdate')
end

require('paq')({
  { 'savq/paq-nvim', opt = true }, -- Let Paq manage itself
  'numToStr/comment.nvim',
  'doums/coBra',
  -- 'doums/ponton.nvim',
  -- 'doums/suit.nvim',
  -- 'doums/espresso',
  'doums/sae',
  'doums/lsp_spinner.nvim',
  'doums/floaterm.nvim',
  'doums/oterm.nvim',
  'doums/vassal.nvim',
  { 'nvim-treesitter/nvim-treesitter', run = update_ts_parsers },
  'nvim-treesitter/playground',
  'neovim/nvim-lspconfig',
  'jose-elias-alvarez/null-ls.nvim',
  'ray-x/lsp_signature.nvim',
  'simrat39/rust-tools.nvim',
  'hrsh7th/nvim-cmp',
  'hrsh7th/cmp-buffer',
  'hrsh7th/cmp-nvim-lua',
  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/cmp-path',
  'saadparwaiz1/cmp_luasnip',
  'L3MON4D3/LuaSnip',
  'folke/trouble.nvim',
  'nvim-lua/plenary.nvim', -- dep of telescope.nvim, gitsigns.nvim, null-ls.nvim
  'nvim-lua/popup.nvim', -- dep of telescope.nvim
  'kkharji/sqlite.lua', -- dep of telescope-smart-history.nvim
  'nvim-telescope/telescope-smart-history.nvim', -- dep of telescope.nvim
  'nvim-telescope/telescope.nvim',
  'lewis6991/gitsigns.nvim',
  'pantharshit00/vim-prisma',
  'kyazdani42/nvim-tree.lua',
  'kyazdani42/nvim-web-devicons', -- dep of nvim-tree.lua
  'ggandor/lightspeed.nvim',
  'AckslD/nvim-neoclip.lua',
  'windwp/nvim-ts-autotag',
  -- paq 'henriquehbr/nvim-startup.lua'
})

-- Init modules --------------------------------------------------

-- create autocmds
require('autocmd')

-- utils
local hl = require('utils').hl

-- OPTIONS -------------------------------------------------------
opt.termguicolors = true
opt.number = true
opt.relativenumber = true
opt.showmode = false
opt.shortmess = 'IFaWcs'
opt.ignorecase = true
opt.smartcase = true
opt.cindent = true
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.showmatch = true
opt.matchtime = 3
opt.updatetime = 100
opt.splitbelow = true
opt.splitright = true
opt.hidden = true
opt.cursorline = true
opt.cursorlineopt = { 'number', 'screenline' }
opt.switchbuf = 'usetab'
opt.scrolloff = 1
opt.completeopt = { 'menuone', 'noselect' }
opt.pumheight = 10
opt.fillchars = { diff = ' ', fold = ' ', eob = ' ', vert = ' ', horiz = ' ' }
opt.complete = opt.complete:append({ 'i' })
opt.clipboard = 'unnamedplus'
opt.signcolumn = 'yes:2'
opt.cmdheight = 2
opt.mouse = 'a'
opt.statusline = ' ' -- hide the default statusline on the first frames
opt.laststatus = 3
opt.guifont = 'JetBrains Mono:h16'
opt.guicursor = 'a:block-Caret'
opt.spelllang = 'en_us'
opt.spelloptions = 'camel'
opt.colorcolumn = '66'
opt.textwidth = 66
opt.formatoptions = opt.formatoptions:append('lv')
opt.foldmethod = 'expr'
opt.foldexpr = 'nvim_treesitter#foldexpr()'
opt.foldlevelstart = 99

-- VARIOUS -------------------------------------------------------
-- color scheme
cmd('colorscheme espresso')
-- nvim as man pager
cmd('runtime ftplugin/man.vim')
-- map leader
g.mapleader = ','
-- highlight group for guicursor
hl('Caret', '#2A211C', '#889AFF', 'bold')
hl('WinSeparator', '#332a25', '#332a25')

-- MAPPINGS ------------------------------------------------------
-- c'est en forgeant que l'on devient forgeron
map('', '<Up>', '<Nop>')
map('', '<Down>', '<Nop>')
map('', '<Right>', '<Nop>')
map('', '<Left>', '<Nop>')
-- move fast with Ctrl + hjkl
map('', '<C-l>', '<Plug>SaeRight', { remap = true })
map('', '<C-h>', '<Plug>SaeLeft', { remap = true })
map('', '<C-j>', '<C-d>')
map('', '<C-k>', '<C-u>')
-- move through wrapped line
map('', 'j', 'gj', { silent = true })
map('', 'k', 'gk', { silent = true })
-- goto start and end of line
map('', '<space>l', '$')
map('', '<space>h', '0')
-- insert mode
map('i', '<A-BS>', '<C-w>')
map('i', '<M-Left>', '<S-Left>')
map('i', '<M-Right>', '<S-Right>')
-- command line
map('c', '<A-Right>', '<C-Right>')
map('c', '<A-Left>', '<C-Left>')
map('c', '<A-BS>', '<C-w>')
-- work inner by default
map('o', 'w', 'iw')
-- search and replace
map('v', '<Leader>f', '<Esc>:%s/\\%V')
map('n', '<Leader>f', ':%s/')
-- hide highlight after a search
map('n', '<space>', ':nohlsearch<CR>', { silent = true })
-- show trailing whitespaces
map('n', '<Leader><Space>', '/\\s\\+$<CR>')
-- tabs
map('n', '<Leader>t', ':tabnew<CR>')
map('', '<C-Right>', ':tabn<CR>', { silent = true })
map('', '<C-Left>', ':tabp<CR>', { silent = true })
map('', '<C-Up>', ':+tabmove<CR>', { silent = true })
map('', '<C-Down>', ':-tabmove<CR>', { silent = true })
-- windows
map('n', '<Leader>s', ':new<CR>', { silent = true })
map('n', '<Leader>v', ':vnew<CR>', { silent = true })
map('n', '<Leader><S-s>', ':split<CR>', { silent = true })
map('n', '<Leader><S-v>', ':vsplit<CR>', { silent = true })
map('n', '<A-h>', '<C-w>h', { silent = true })
map('n', '<A-l>', '<C-w>l', { silent = true })
map('n', '<A-j>', '<C-w>j', { silent = true })
map('n', '<A-k>', '<C-w>k', { silent = true })
map('n', '<A-Up>', ':resize +4<CR>', { silent = true })
map('n', '<A-Down>', ':resize -4<CR>', { silent = true })
map('n', '<A-Right>', ':vertical resize +4<CR>', { silent = true })
map('n', '<A-Left>', ':vertical resize -4<CR>', { silent = true })
-- terminal normal mode
map('t', '<Leader>n', '<C-\\><C-N>')
-- toggle spell check
map(
  'n',
  '<F4>',
  [[:lua vim.opt.spell = not vim.opt.spell:get()<CR>]],
  { silent = true }
)

-- LOAD PLUGIN CONFIGS -------------------------------------------
require('plugins.nvim-treesitter')
require('plugins.ponton')
require('plugins.nvim-tree')
require('plugins.vassal')
require('plugins.comment')
require('plugins.telescope')
require('plugins.cobra')
require('plugins.floaterm')
require('plugins.trouble')
require('plugins.nvim-cmp')
require('plugins.luasnip')
require('lsp')

-- gitsigns.nvim -------------------------------------------------
-- git-menu
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

require('gitsigns').setup({
  signs = {
    add = { hl = 'GitAddSign', text = '┃' },
    change = { hl = 'GitChangeSign', text = '┃' },
    delete = { hl = 'GitDeleteSign', text = '▶' },
    topdelete = { hl = 'GitDeleteSign', text = '▶' },
    changedelete = { hl = 'GitChangeDeleteSign', text = '┃' },
  },
  numhl = false,
  linehl = false,
  current_line_blame_formatter = '<author>, <author_time:%d-%m-%Y> - <summary>',
  on_attach = function(bufnr)
    local opts = { buffer = bufnr }
    map('n', '<A-g>', git_menu, opts)
    map('n', '<leader>n', gs.next_hunk, opts)
    map('n', '<leader>N', gs.prev_hunk, opts)
  end,
  preview_config = { border = { '', '', '', ' ', '', '', '', ' ' } },
})

-- lightspeed ----------------------------------------------------
-- stylua: ignore start
require('lightspeed').setup({
  exit_after_idle_msecs = { labeled = 1500, unlabeled = 1500 },
  safe_labels = { 's', 'f', 'n', 'u', 't', 'b', 'g', 'F',
    'L', 'N', 'H', 'G', 'M', 'U', 'T', 'Z' },
  labels = { 's', 'f', 'n', 'g', 'h', 'v', 'b', 'w', 'y', 'd',
    'q', 'z', 'c', 'x', 't', 'u', 'r', 'i', 'a', 'o', 'e' },
  repeat_ft_with_target_char = true
})
-- stylua: ignore end
hl('LightspeedCursor', '#212121', '#ebff00', 'bold')
hl('LightspeedLabel', '#f49810', nil, { 'bold', 'underline' })
hl('LightspeedLabelOverlapped', '#f49810', nil, 'underline')
hl('LightspeedShortcut', '#212121', '#f49810', { 'bold', 'underline' })
hl('LightspeedOneCharMatch', '#212121', '#f49810', 'bold')
hl('LightspeedGreyWash', '#80807f')
hl('LightspeedUnlabeledMatch', '#ddddff', nil, 'bold')
hl('LightspeedPendingOpArea', '#212121', '#f49810')
hl('LightspeedLabelDistant', '#aa4e00', nil, { 'bold', 'underline' })
hl('LightspeedLabelDistantOverlapped', '#aa4e00', nil, 'underline')
hl('LightspeedMaskedChar', '#906526')

-- api.nvim_del_keymap('n', 't')

-- nvim-neoclip.lua ----------------------------------------------
require('neoclip').setup()
map(
  '',
  '<A-c>',
  [[:lua require('telescope').extensions.neoclip.plus()<cr>]],
  { silent = true }
)

-- suit.nvim -----------------------------------------------------
hl('suitPrompt', '#C7C7FF', '#1d1916', { 'bold', 'italic' })
hl('suitInput', '#BDAE9D', '#1d1916')
hl('suitSelectedItem', nil, '#3b2f27')
require('suit').setup({
  input = {
    default_prompt = '↓',
    border = 'vgap',
    hl_prompt = 'suitPrompt',
    hl_input = 'suitInput',
    hl_border = 'suitInput',
  },
  select = {
    default_prompt = '≡',
    border = 'vgap',
    hl_prompt = 'suitPrompt',
    hl_select = 'suitInput',
    hl_border = 'suitInput',
    hl_selected_item = 'suitSelectedItem',
  },
})
