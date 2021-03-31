local fn = vim.fn

-- PLUGINS -------------------------------------------------------
-- auto install paq-nvim
local install_path = fn.stdpath('data')..'/site/pack/paqs/opt/paq-nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  vim.cmd('!git clone https://github.com/savq/paq-nvim.git '..install_path)
end

vim.cmd 'packadd paq-nvim'         -- Load package
local paq = require'paq-nvim'.paq  -- Import module and bind `paq` function
paq {'savq/paq-nvim', opt=true}    -- Let Paq manage itself

-- update treesitter parsers
local function update_ts_parsers() vim.cmd 'TSUpdate' end

paq 'b3nj5m1n/kommentary'
paq 'airblade/vim-gitgutter'
paq 'dense-analysis/ale'
paq 'doums/barow'
paq 'doums/coBra'
paq 'doums/oterm'
paq 'doums/nnnvi'
paq 'doums/fzfTools'
paq 'doums/darcula'
paq 'doums/sae'
paq 'doums/barowLSP'
paq 'doums/barowGit'
paq 'doums/rgv'
paq {'neoclide/coc.nvim', branch='release'}
paq {'nvim-treesitter/nvim-treesitter', run=update_ts_parsers}
paq 'nvim-treesitter/playground'
------------------------------------------------------------------

-- HELPERS -------------------------------------------------------
--[[ make buffer and window option global as well
     TODO use vim.opt when released
     see https://github.com/neovim/neovim/pull/13479 ]]
local function opt(key, value, scope)
  scope = scope or 'o'
  local scopes = {o = vim.o, b = vim.bo, w = vim.wo}
  scopes[scope][key] = value
  if scope ~= 'o' then scopes['o'][key] = value end
end

-- `t` for `termcodes`.
local function t(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

-- use `nvim_set_keymap` with `noremap` option set to `true` by default
local function map(mode, lhs, rhs, opts)
  opts = opts or {noremap = true}
  if opts.noremap == nil then opts.noremap = true end
  vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
end
------------------------------------------------------------------

-- OPTIONS -------------------------------------------------------
opt('termguicolors', true)
opt('number', true, 'w')
opt('relativenumber', true, 'w')
opt('showmode', false)
opt('shortmess', 'IFaWcs')
opt('ignorecase', true)
opt('smartcase', true)
opt('cindent', true, 'b')
opt('tabstop', 2, 'b')
opt('shiftwidth', 2, 'b')
opt('expandtab', true, 'b')
opt('showmatch', true)
opt('matchtime', 3)
opt('updatetime', 100)
opt('splitbelow', true)
opt('splitbelow', true)
opt('splitright', true)
opt('foldlevelstart', 0)
opt('hidden', true)
opt('cursorline', true, 'w')
opt('switchbuf', 'usetab')
opt('scrolloff', 5, 'w')
opt('completeopt', 'menuone')
opt('pumheight', 10)
opt('fillchars', 'vert: ,diff: ,fold: ', 'w')
opt('complete', vim.bo.complete..',i', 'b')
opt('clipboard', 'unnamedplus')
opt('guicursor', '')
------------------------------------------------------------------

-- color scheme
vim.cmd 'colorscheme darcula'
-- nvim as man pager
vim.cmd 'runtime ftplugin/man.vim'
-- map leader
vim.g.mapleader = ','

-- MAPPINGS ------------------------------------------------------
-- c'est en forgeant que l'on devient forgeron
map('', '<Up>', '<Nop>')
map('', '<Down>', '<Nop>')
map('', '<Right>', '<Nop>')
map('', '<Left>', '<Nop>')
-- move fast with Ctrl + hjkl
map('', '<C-l>', '<Plug>SaeRight', {noremap=false})
map('', '<C-h>', '<Plug>SaeLeft', {noremap=false})
map('', '<C-j>', '<C-d>')
map('', '<C-k>', '<C-u>')
-- move through wrapped line
map('', 'j', 'gj', {silent=true})
map('', 'k', 'gk', {silent=true})
-- goto start and end of line
map('', '<space>l', '$')
map('', '<space>h', '0')
-- work inner by default
map('o', 'w', 'iw')
-- search and replace
map('v', '<Leader>f', '<Esc>:%s/\\%V')
map('n', '<Leader>f', ':%s/')
-- hide highlight after a search
map('n', '<space>', ':nohlsearch<CR>', {silent=true})
-- show trailing whitespaces
map('n', '<Leader><Space>', '/\\s\\+$<CR>')
-- tabs
map('n', '<Leader>t', ':tabnew<CR>')
map('', '<C-Right>', ':tabn<CR>', {silent=true})
map('', '<C-Left>', ':tabp<CR>', {silent=true})
map('', '<C-Up>', ':+tabmove<CR>', {silent=true})
map('', '<C-Down>', ':-tabmove<CR>', {silent=true})
-- windows
map('n', '<Leader>s', ':new<CR>', {silent=true})
map('n', '<Leader>v', ':vnew<CR>', {silent=true})
map('n', '<Leader><S-s>', ':split<CR>', {silent=true})
map('n', '<Leader><S-v>', ':vsplit<CR>', {silent=true})
map('n', '<A-h>', '<C-w>h', {silent=true})
map('n', '<A-l>', '<C-w>l', {silent=true})
map('n', '<A-j>', '<C-w>j', {silent=true})
map('n', '<A-k>', '<C-w>k', {silent=true})
map('n', '<A-Up>', ':resize +4<CR>', {silent=true})
map('n', '<A-Down>', ':resize -4<CR>', {silent=true})
map('n', '<A-Right>', ':vertical resize +4<CR>', {silent=true})
map('n', '<A-Left>', ':vertical resize -4<CR>', {silent=true})
-- terminal normal mode
map('t', '<Leader>n', '<C-\\><C-N>')
------------------------------------------------------------------

-- barow --------------------------------------------------------
vim.g.barow = {
  modules = {
    {'barowGit#branch', 'BarowHint'},
    {'barowLSP#error', 'BarowError'},
    {'barowLSP#warning', 'BarowWarning'},
    {'barowLSP#info', 'BarowInfo'},
    {'barowLSP#hint', 'BarowHint'},
    {'barowLSP#coc_status', 'Barow'},
    {'barowLSP#ale_status', 'Barow'}
  }
}
-- TODO
-- hi! link StatusLine Barow
-- hi! link StatusLineNC BarowNC
------------------------------------------------------------------

-- kommentary ----------------------------------------------------
vim.g.kommentary_create_default_mappings = false
vim.api.nvim_set_keymap("n", "<leader>cc", "<Plug>kommentary_line_default", {})
vim.api.nvim_set_keymap("n", "<leader>c", "<Plug>kommentary_motion_default", {})
vim.api.nvim_set_keymap("v", "<leader>c", "<Plug>kommentary_visual_default", {})
------------------------------------------------------------------

-- coBra ---------------------------------------------------------
vim.g.coBraPairs = {
  rust = {
    {'<', '>'},
    {'"', '"'},
    {'{', '}'},
    {'(', ')'},
    {'[', ']'}
  }
}
------------------------------------------------------------------

-- OTerm ---------------------------------------------------------
map('n', '<Leader>o', '<Plug>OTerm', {noremap=false})
------------------------------------------------------------------

-- fzfTools ------------------------------------------------------
vim.g.fzfTools = {
  gitlog = {tab = 1},
  gitlogsel = {tab = 1},
}
map('n', '<C-s>', '<Plug>Ls', {noremap=false})
map('n', '<C-b>', '<Plug>Buffers', {noremap=false})
map('n', '<A-p>', '<Plug>Registers', {noremap=false})
map('n', '<C-g>', '<Plug>GitLog', {noremap=false})
map('n', '<C-g>', '<Plug>SGitLog', {noremap=false})
------------------------------------------------------------------

-- nnnvi ---------------------------------------------------------
vim.g.nnnvi = {
  layout = {left = 40, min = 30},
  maps = {
    ['<A-s>'] = 'split',
    ['<A-v>'] = 'vsplit',
    ['<A-a>'] = 'tabedit',
  }
}
map('n', '<Tab>', '<Plug>NNNs', {noremap=false})
map('n', '<S-Tab>', '<Plug>NNNnos', {noremap=false})
------------------------------------------------------------------

-- rgv -----------------------------------------------------------
map('n', '<A-o>', '<Plug>RgToggle', {noremap=false})
------------------------------------------------------------------
