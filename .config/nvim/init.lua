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
  * cspell (npm package)

others: git, ripgrep, fzf, node, npm
-- ]]

-- ALIASES -------------------------------------------------------
local fn = vim.fn
local cmd = vim.cmd
local g = vim.g
local o = vim.o
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
  -- 'doums/espresso.nvim',
  -- 'doums/modeui.nvim'
  'doums/sae',
  'doums/lsp_spinner.nvim',
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
  'ggandor/leap.nvim',
  'AckslD/nvim-neoclip.lua',
  'windwp/nvim-ts-autotag',
})

-- OPTIONS -------------------------------------------------------
o.termguicolors = true
o.number = true
o.relativenumber = true
o.showmode = false
o.shortmess = 'IFaWcs'
o.ignorecase = true
o.smartcase = true
o.cindent = true
o.tabstop = 2
o.shiftwidth = 2
o.expandtab = true
o.showmatch = true
o.matchtime = 3
o.updatetime = 100
o.splitbelow = true
o.splitright = true
o.hidden = true
o.cursorline = true
o.cursorlineopt = 'number,screenline'
o.switchbuf = 'usetab'
o.scrolloff = 1
o.completeopt = 'menuone,noselect'
o.pumheight = 10
o.fillchars = 'diff: ,fold: ,eob: ,vert: ,horiz: ,lastline:•'
o.clipboard = 'unnamedplus'
o.signcolumn = 'yes:2'
o.cmdheight = 2
o.mouse = 'a'
o.statusline = ' ' -- hide the default statusline on the first frames
o.laststatus = 3
o.guifont = 'JetBrains Mono:h16'
o.guicursor = 'a:block-Caret'
o.spelllang = 'en_us'
o.spelloptions = 'camel'
o.colorcolumn = '66'
o.textwidth = 66
o.foldmethod = 'expr'
o.foldexpr = 'nvim_treesitter#foldexpr()'
o.foldlevelstart = 99
o.splitkeep = 'screen'
opt.complete = opt.complete:append({ 'i' })
opt.formatoptions = opt.formatoptions:append('lv')

-- create autocmds
require('autocmd')

-- VARIOUS -------------------------------------------------------
-- color scheme
cmd('colorscheme espresso')
-- nvim as man pager
cmd('runtime ftplugin/man.vim')
-- map leader
g.mapleader = ','

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
-- reload nvim config
map('n', '<F10>', function()
  vim.cmd([[
      update $MYVIMRC
      source $MYVIMRC
    ]])
  vim.notify(
    'config reloaded ✓',
    vim.log.levels.INFO,
    { title = 'nvim-config' }
  )
end)

-- generate custom highlight groups
require('hl').hl()

-- LOAD PLUGIN CONFIGS -------------------------------------------
require('plugins.nvim-treesitter')
require('plugins.ponton')
require('plugins.nvim-tree')
require('plugins.vassal')
require('plugins.comment')
require('plugins.telescope')
require('plugins.cobra')
require('plugins.oterm')
require('plugins.trouble')
require('plugins.nvim-cmp')
require('plugins.luasnip')
require('plugins.gitsigns')
require('plugins.leap')
require('plugins.nvim-neoclip')
require('plugins.suit')
require('plugins.modeui')
require('lsp')
