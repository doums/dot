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
local function update_ts_parsers() vim.cmd('TSUpdate') end

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
------------------------------------------------------------------

-- kommentary ----------------------------------------------------

vim.g.kommentary_create_default_mappings = false
vim.api.nvim_set_keymap("n", "<leader>cc", "<Plug>kommentary_line_default", {})
vim.api.nvim_set_keymap("n", "<leader>c", "<Plug>kommentary_motion_default", {})
vim.api.nvim_set_keymap("v", "<leader>c", "<Plug>kommentary_visual_default", {})
------------------------------------------------------------------
