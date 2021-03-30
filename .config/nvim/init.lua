-- auto install paq-nvim
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/paqs/opt/paq-nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  vim.cmd('!git clone https://github.com/savq/paq-nvim.git '..install_path)
end

vim.cmd 'packadd paq-nvim'         -- Load package
local paq = require'paq-nvim'.paq  -- Import module and bind `paq` function
paq {'savq/paq-nvim', opt=true}    -- Let Paq manage itself

-- update treesitter parsers
local function updateTSParsers() vim.cmd('TSUpdate') end

-- plugins
paq 'scrooloose/nerdcommenter'
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
paq {'nvim-treesitter/nvim-treesitter', run=updateTSParsers}
paq 'nvim-treesitter/playground'

-- options


-- color scheme
vim.cmd 'colorscheme darcula'

-- nvim as man pager
vim.cmd 'runtime ftplugin/man.vim'

