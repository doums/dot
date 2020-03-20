if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'itchyny/lightline.vim'
Plug 'dag/vim-fish'
Plug 'scrooloose/nerdcommenter'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'airblade/vim-gitgutter'
Plug 'leafgarland/typescript-vim'
Plug 'dense-analysis/ale'
Plug 'tpope/vim-fugitive'
Plug 'rust-lang/rust.vim'
Plug 'doums/coBra'
Plug 'doums/fzfTools'
Plug 'doums/darcula'
Plug 'doums/sae'
Plug 'jparise/vim-graphql'
Plug 'Shougo/deoplete.nvim'
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'

call plug#end()

" vanilla config {{{
let mapleader = ","
set termguicolors
set number
colorscheme darcula
filetype plugin on
filetype indent on
syntax on
set nocompatible
set showcmd
set noshowmode
set laststatus=2
set shortmess=fFaWcs
set ignorecase
set smartcase
set cindent
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
set startofline
set foldlevelstart=0
set textwidth=0
set hidden
set cursorline
set switchbuf=usetab
set scrolloff=5
set completeopt=menuone,popup
set fillchars=diff:\ " a space
set completepopup=border:off
set autoread
set display=lastline
set wildmenu
set clipboard=unnamedplus,autoselect,exclude:cons\|linux
runtime! ftplugin/man.vim
" }}}

" plugins config {{{

" deoplete
let g:deoplete#enable_at_startup = 1

" netrw
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_winsize = 30

" NERDCommenter
let g:NERDCreateDefaultMappings = 0
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1

" lightline.vim
let g:lightline = {
      \ 'colorscheme': 'darculaOriginal',
			\ 'component_function': {
			\   'gitbranch': 'fugitive#head'
			\ },
      \ 'component_expand': {
      \   'ale_ok': 'lla#Ok',
      \   'ale_cheking': 'lla#Checking',
      \   'ale_error': 'lla#Errors',
      \   'ale_warning': 'lla#Warnings'
      \ },
      \ 'component_type': {
      \   'ale_ok': 'ok',
      \   'ale_cheking': 'middle',
      \   'ale_error': 'error',
      \   'ale_warning': 'warning'
      \ }
      \ }
let g:lightline.active = {
      \ 'right': [
      \   [ 'lineinfo' ],
      \   [ 'percent' ],
      \   [
      \     'gitbranch',
      \     'fileformat',
      \     'fileencoding',
      \     'filetype',
      \     'ale_cheking',
      \     'ale_ok',
      \     'ale_warning',
      \     'ale_error'
      \   ]
      \ ]
      \ }
let g:lightline.tab = {
      \ 'active': [ 'filename', 'modified' ],
      \ 'inactive': [ 'filename', 'modified' ]
      \ }
let g:lightline.tabline = {
      \ 'left': [ [ 'tabs' ] ],
      \ 'right': []
      \ }
let g:lightline.tabline_subseparator = {
      \ 'left': '',
      \ 'right': ''
      \ }

" GitGutter
let g:gitgutter_enabled = 0

" typescript-vim
let g:typescript_indent_disable = 1

" ALE
let g:ale_set_highlights = 0
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_info_str = 'I'
let g:ale_echo_msg_format = '[%linter%][%severity%] %s'
let g:ale_fixers = {
      \ 'javascript': [ 'eslint' ],
      \ 'json': [ 'eslint', 'standard' ],
      \ 'typescript': [ 'eslint' ],
      \ 'graphql': [ 'eslint' ],
      \ 'rust': [ 'rustfmt' ]
      \ }
let g:ale_linters = {
      \ 'javascript': [ 'eslint', 'standard' ],
      \ 'json': [ 'eslint', 'standard' ],
      \ 'typescript': [ 'eslint', 'tsserver' ],
      \ 'graphql': [ 'eslint '],
      \ 'rust': [ 'rls', 'rustfmt', 'cargo' ]
      \ }
let g:ale_linters_explicit = 1
let g:ale_fix_on_save = 1

" coBra
let g:rust_keep_autopairs_default = 0
let g:coBraPairs = {
      \ 'rust': [
      \    ['<', '>'],
      \    ['"', '"'],
      \    ['{', '}'],
      \    ['(', ')'],
      \    ['[', ']']
      \    ]
      \ }
" }}}

" vanilla mapping {{{

" c'est en forgeant que l'on devient forgeron
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Right> <Nop>
noremap <Left> <Nop>
" NORMAL move fast with Ctrl + hjkl
nmap <C-l> <Plug>SaeRight
nmap <C-h> <Plug>SaeLeft
noremap <C-j> <C-d>
noremap <C-k> <C-u>
" VISUAL move fast with Ctrl + hjkl
vmap <C-l> <Plug>SaeRight
vmap <C-h> <Plug>SaeLeft
vnoremap <C-j> <C-d>
vnoremap <C-k> <C-u>
" NORMAL/VISUAL/OP_P move through wrapped line
noremap <silent> j gj
noremap <silent> k gk
" remap goto begin and end of line
noremap <Space>h 0
noremap <Space>l $
" work inner by default
onoremap w iw
" copy in/past from "a register
noremap <Leader>y ay
nnoremap <Leader>i ayiw
nnoremap <Leader>p ap
nnoremap <Leader>P aP
" search and replace
vnoremap <Leader>f <Esc>:%s/\%V
nnoremap <Leader>f :%s/
" hide highlight after a search
nnoremap <silent> <space> :nohlsearch<CR>
" select all
noremap <silent> <C-a> ggvG$
" show trailing whitespaces
nnoremap <Leader><Space> /\s\+$<CR>
" tab
nnoremap <Leader>t :tabnew<CR>
noremap <silent> <C-Right> :tabn<CR>
noremap <silent> <C-Left> :tabp<CR>
nnoremap <silent> <C-Up> :+tabmove<CR>
nnoremap <silent> <C-Down> :-tabmove<CR>
" window
nnoremap <silent><Leader>s :new<CR>
nnoremap <silent><Leader>v :vnew<CR>
nnoremap <silent><Leader><S-s> :split<CR>
nnoremap <silent><Leader><S-v> :vsplit<CR>
nnoremap <silent> <h <C-w>h
nnoremap <silent> <l <C-w>l
nnoremap <silent> <k <C-w>k
nnoremap <silent> <j <C-w>j
nnoremap <silent> <Leader><Left> <C-w>H
nnoremap <silent> <Leader><Down> <C-w>J
nnoremap <silent> <Leader><Up> <C-w>K
nnoremap <silent> <Leader><Right> <C-w>L
noremap <Leader>= <C-w>=
nnoremap <silent> ²j :resize +4<CR>
nnoremap <silent> ²k :resize -4<CR>
nnoremap <silent> ²h :vertical :resize +4<CR>
nnoremap <silent> ²l :vertical :resize -4<CR>
" replace the word under the cursor
" by the first or the selected completion suggestion
inoremap <expr> <Tab> <SID>Complete()
" terminal mode
tnoremap <Leader>n <C-W>N
nnoremap <silent> <Down> :term<CR>
" }}}

" {{{ plugins mapping

" GitGutter
nnoremap <silent> <Leader>g :GitGutterToggle<CR>
" fzfTools
nmap <C-s> <Plug>Ls
nmap <C-b> <Plug>Buf
" netrw
nnoremap <silent> <Tab> :Lex<CR>
" NERDCommenter
map <Leader>c <plug>NERDCommenterToggle
map <Leader><S-c> <plug>NERDCommenterSexy
" Ale
nmap <Leader>a <Plug>(ale_toggle)
imap <C-@> <Plug>(ale_complete)
nmap <Leader>b <Plug>(ale_go_to_definition)
nmap <Leader>n <Plug>(ale_go_to_type_definition)
nmap <Leader>r <Plug>(ale_find_references)
nmap <Leader>d <Plug>(ale_detail)
nnoremap <Leader>: :ALESymbolSearch
map <C-q> <Plug>(ale_hover)
nmap <silent> <C-g> <Plug>(ale_previous_wrap)
nmap <silent> <C-G> <Plug>(ale_next_wrap)
" }}}

" autocommand {{{
augroup stuff
autocmd!
" whenever CursorHold is fired (nothing typed during 'updatetime')
" run checktime to refresh the buffer and retrieve any external changes
autocmd CursorHold * checktime %
" set fold to marker for .vimrc
autocmd FileType vim setlocal foldmethod=marker
" set stuff for some programming languages
autocmd FileType * call s:CodeStuff()
autocmd FileType man set nonumber
" when browsing whitin netrw, map cw to gncd -> make the dir under
" the cursor the new tree top and set the current working dir to it
autocmd FileType netrw nmap <buffer><silent> cw gncd
" terminal stuff
" autocmd TerminalOpen,TerminalWinOpen * call s:InitTermSetUp()
" autocmd WinLeave,BufLeave * call s:RestoreSetUp()
" autocmd WinEnter,BufEnter * call s:InitTermSetUp()
" autocmd TerminalWinOpen * call s:NewTerm()
augroup END
" }}}

" darcula override {{{
hi! link rustQuestionMark PreProc
hi! link rustMacro PreProc
call darcula#Hi('rustLifetime', darcula#palette.macroName, darcula#palette.bg, 'italic')
call darcula#Hi('rustTypeParameter', darcula#palette.macroName, darcula#palette.bg, 'bold')
" }}}

" {{{ functions
function s:Complete()
  let infos = complete_info()
  if infos.pum_visible == 1 && !empty(infos.items)
    if infos.selected < 0
      let idx = 0
    else
      let idx = infos.selected
    endif
    if empty(infos.items[idx].abbr)
      return "\<Left>\<C-o>diw".infos.items[idx].word
    else
      return "\<Left>\<C-o>diw".infos.items[idx].abbr
    endif
  else
    return "\<Tab>"
  endif
endfunction

function s:CodeStuff()
  if &filetype == "rust"
    nnoremap <buffer> <Leader>; iprintln!("")<Esc><Left>i
    inoremap <buffer> <Leader>; println!("")<Esc><Left>i
    nnoremap <buffer> <F5> :write<CR>:Cargo run<CR>
    nnoremap <buffer> <F4> :write<CR>:Cargo test<CR>
  endif
  if &filetype == "javascript" || &filetype == "typescript"
    nnoremap <buffer> <Leader>; iconsole.log('')<Esc><Left>i
    inoremap <buffer> <Leader>; console.log('')<Esc><Left>i
  endif
endfunction
" }}}

" {{{ fix alacritty colors
if &term == "alacritty"
  let &term = "xterm-256color"
endif
" }}}

" {{{ scraps
" noremap <F9> :call <SID>DebugHi()<CR>
" nnoremap <F5> :source $MYVIMRC<CR>

" autocmd CursorMoved * call s:DebugHi()

" noremap <A-y> :call <SID>Log()<CR>

function s:DebugHi()
  let name = synID(line("."), col("."), 1)->synIDattr("name")
  let link= synID(line("."), col("."), 1)->synIDtrans()->synIDattr("name")
  let fg = synID(line("."), col("."), 1)->synIDtrans()->synIDattr("fg")
  let bg = synID(line("."), col("."), 1)->synIDtrans()->synIDattr("bg")
  echo 'hi: '.name.', link: '.link.', bg: '.bg.', fg: '.fg
endfunction
" }}}
