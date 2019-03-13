if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox'
Plug 'junegunn/fzf.vim'
Plug 'dag/vim-fish'
Plug 'scrooloose/nerdcommenter'
Plug 'doums/lightlineGruvbox'

call plug#end()

let mapleader = ","
set termguicolors
set number
let g:gruvbox_italic=1
set background=dark
colorscheme gruvbox
filetype plugin on
syntax on
set nocompatible
set showcmd
set noshowmode
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'gruvbox',
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
set showmatch
set matchtime=3
let g:NERDCreateDefaultMappings = 0
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
nnoremap <silent> <C-s> :Files<CR>
nnoremap <silent> <space> :nohlsearch<CR>
map <C-n> :NERDTreeToggle<CR>
map <Leader>c <plug>NERDCommenterToggle
map <Leader><S-c> <plug>NERDCommenterSexy
nmap <Up> <Nop>
nmap <Down> <Nop>
nmap <Right> <Nop>
nmap <Left> <Nop>
