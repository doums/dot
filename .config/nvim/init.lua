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
})

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
require('plugins.floaterm')
require('plugins.trouble')
require('plugins.nvim-cmp')
require('plugins.luasnip')
require('plugins.gitsigns')
require('plugins.lightspeed')
require('plugins.nvim-neoclip')
require('plugins.suit')
require('lsp')
