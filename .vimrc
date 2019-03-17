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
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'airblade/vim-gitgutter'
Plug 'leafgarland/typescript-vim'
Plug 'w0rp/ale'

call plug#end()

" main config
let mapleader = ","
set termguicolors
set number
set background=dark
colorscheme gruvbox
filetype plugin on
syntax on
set nocompatible
set showcmd
set noshowmode
set laststatus=2
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
set updatetime=100
set splitbelow
set splitright

" plugins config
let g:gruvbox_italic=1
let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeMapPreviewSplit = '<Leader>s'
let g:NERDTreeMapPreviewVSplit = '<Leader>v'
let g:NERDTreeMapOpenSplit = 's'
let g:NERDTreeMapOpenVSplit = 'v'
let g:NERDCreateDefaultMappings = 0
let g:NERDCreateDefaultMappings = 0
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ }
set shortmess=fFaWcs
let g:gitgutter_enabled = 0
let g:typescript_indent_disable = 1
let b:ale_fixers = {
      \ 'javascript': [ 'eslint' ],
      \ 'typescript': [ 'eslint' ]
      \ }
let b:ale_linters = {
      \ 'javascript': [ 'eslint' ],
      \ 'typescript': [ 'eslint' ]
      \ }
let g:ale_linters_explicit = 1
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit' }

" mapping
nnoremap <Leader>y "ay
nnoremap <Leader>i "ayiw
vnoremap <Leader>y "ay
nnoremap <Leader>p "ap
nnoremap é <Home>
vnoremap é <Home>
nnoremap " <End>
vnoremap " <End>
nnoremap <silent> <C-s> :Files<CR>
nnoremap <silent> <space> :nohlsearch<CR>
noremap <silent> <C-n> :NERDTreeToggle<CR>
map <Leader>c <plug>NERDCommenterToggle
map <Leader><S-c> <plug>NERDCommenterSexy
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Right> <Nop>
noremap <Left> <Nop>
nnoremap <silent> <Leader>g :GitGutterToggle<CR>
nnoremap <Leader>b :ALEGoToDefinition<CR>
nnoremap <Leader>r :ALEFindReferences<CR>
nnoremap <Leader>t :tabnew<CR>
nnoremap <silent> <C-Right> :tabn<CR>
nnoremap <silent> <C-Left> :tabp<CR>
nnoremap <C-o> :tabo<CR>
nnoremap <Leader>s :new<CR>
nnoremap <Leader>v :vnew<CR>
nnoremap <Leader><S-s> :split<CR>
nnoremap <Leader><S-v> :vsplit<CR>
nnoremap <Leader>o :only<CR>
nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-l> <C-w>l
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-j> <C-w>j
nnoremap <Leader>= <C-w>=
nnoremap <silent> <Leader>< :resize +4<CR>
nnoremap <silent> <Leader>> :resize -4<CR>
nnoremap <silent> <Leader>w :vertical :resize +4<CR>
nnoremap <silent> <Leader><S-w> :vertical :resize -4<CR>
