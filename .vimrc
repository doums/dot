if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'BrainDeath0/Hypsteria'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox'
Plug 'junegunn/fzf.vim'
Plug 'dag/vim-fish'

call plug#end()

set termguicolors
set number
" let g:gruvbox_italic=1
colo hypsteria
" set background=dark
filetype plugin on
syntax on
set nocompatible
set showcmd
set noshowmode
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'seoul256',
      \ }
set shortmess=fFaWcs
set ignorecase
set smartcase
set tabstop=2
set shiftwidth=2
set expandtab
set smarttab
set hlsearch
set incsearch
nnoremap <silent> <C-s> :Files<CR>
nnoremap <silent> ,<space> :nohlsearch<CR>
map <C-n> :NERDTreeToggle<CR>
