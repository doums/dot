if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

let g:ale_completion_enabled=1

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
Plug 'tpope/vim-fugitive'
Plug 'rust-lang/rust.vim'

call plug#end()

" main config
let mapleader = ","
set termguicolors
set number
let g:gruvbox_italic=1
let g:gruvbox_sign_column='bg0'
set background=dark
colorscheme gruvbox
filetype plugin on
syntax on
" set mouse=a
set nocompatible
set showcmd
set noshowmode
set laststatus=2
set shortmess=fFaWcs
set ignorecase
set smartcase
set autoindent
set tabstop=2
set shiftwidth=2
set expandtab
set hlsearch
set incsearch
set showmatch
set matchtime=3
set updatetime=100
set splitbelow
set splitright
" remove all trailing white space before write
autocmd BufWritePre * %s/\s\+$//e

" plugins config
let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeShowHidden = 1
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
let g:gitgutter_enabled = 0
let g:typescript_indent_disable = 1
let g:ale_set_highlights = 0
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_info_str = 'I'
let g:ale_echo_msg_format = '[%linter%][%severity%] %s'
let g:ale_fixers = {
      \ 'javascript': [ 'eslint' ],
      \ 'typescript': [ 'eslint', 'tsserver' ],
      \ 'graphql': [ 'eslint ']
      \ }
let g:ale_linters = {
      \ 'javascript': [ 'eslint', 'standard' ],
      \ 'typescript': [ 'eslint', 'tsserver' ],
      \ 'graphql': [ 'eslint '],
      \ 'rust': [ 'cargo', 'rls', 'rustc', 'clippy', 'rustfmt' ]
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
nnoremap <Leader>P "aP
vnoremap <Leader>: <Esc>:%s/\%V
nnoremap <Leader>: :%s/
nnoremap dw diw
nnoremap cw ciw
nnoremap dW diW
nnoremap cW ciW
nnoremap db dib
nnoremap cb cib
nnoremap dB diB
nnoremap cB ciB
nnoremap d[ di[
nnoremap c[ ci[
nnoremap d< di<
nnoremap c< ci<
nnoremap dt dit
nnoremap ct cit
nnoremap d' di'
nnoremap c' ci'
nnoremap d" di"
nnoremap c" ci"
nnoremap d` di`
nnoremap c` ci`
nnoremap é <Home>
vnoremap é <Home>
nnoremap " <End>
vnoremap " <End>
nnoremap <silent> <C-s> :Files<CR>
nnoremap <silent> <space> :nohlsearch<CR>
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Right> <Nop>
noremap <Left> <Nop>
nnoremap <silent> <F2> :setlocal spell! spelllang=en_us<CR>
nnoremap <silent> <Leader>g :GitGutterToggle<CR>

" NERDTree
noremap <silent> <C-n> :NERDTreeToggle<CR>
map <Leader>c <plug>NERDCommenterToggle
map <Leader><S-c> <plug>NERDCommenterSexy

" Ale
nmap <Leader>a <Plug>(ale_toggle)
imap <C-@> <Plug>(ale_complete)
nmap <Leader>b <Plug>(ale_go_to_definition_in_split)
nmap <Leader>n <Plug>(ale_go_to_type_definition_in_split)
nmap <Leader>r <Plug>(ale_find_references)
nmap <Leader>d <Plug>(ale_detail)
nnoremap <Leader>f :ALESymbolSearch
map <C-q> <Plug>(ale_hover)

" tab
nnoremap <Leader>t :tabnew<CR>
nnoremap <silent> <C-Right> :tabn<CR>
nnoremap <silent> <C-Left> :tabp<CR>

" window
nnoremap <Leader>s :new<CR>
nnoremap <Leader>v :vnew<CR>
nnoremap <Leader><S-s> :split<CR>
nnoremap <Leader><S-v> :vsplit<CR>
nnoremap <Leader>o :only<CR>
nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-l> <C-w>l
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-j> <C-w>j
nnoremap <silent> <Leader><Left> <C-w>H
nnoremap <silent> <Leader><Down> <C-w>J
nnoremap <silent> <Leader><Up> <C-w>K
nnoremap <silent> <Leader><Right> <C-w>L
noremap <Leader>= <C-w>=
nnoremap <silent> <Leader>< :resize +4<CR>
nnoremap <silent> <Leader>> :resize -4<CR>
nnoremap <silent> <Leader>w :vertical :resize +4<CR>
nnoremap <silent> <Leader><S-w> :vertical :resize -4<CR>

" fix gruvbox's highlight for Ale
highlight ALEInfo ctermfg=109 cterm=italic
highlight ALEWarning ctermfg=214 cterm=italic
highlight ALEError ctermfg=167 cterm=italic
" fix spell highlight
highlight SpellBad ctermfg=167 cterm=underline
highlight SpellLocal ctermfg=108 cterm=underline
highlight SpellCap ctermfg=108 cterm=underline
highlight SpellRare ctermfg=108 cterm=underline
