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

" main config {{{
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
set startofline
" }}}

" plugins config {{{
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
let g:rustfmt_autosave = 1
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
" }}}

" mapping {{{
"
" By default Vim assumes that pressing the Alt key sets the 8th bit of a
" typed character. However Terminator don't use this system: when Alt key
" is pressed, the escape sequence is sent.
"
" To map Alt key, while writing the mapping in INSERT mod
" hit Ctrl-v then hit Alt + the key you want
"
" :h map-alt-keys
" :h :map-special-keys

" INSERT move with Alt + hjkl
inoremap h <Left>
inoremap j <Down>
inoremap k <Up>
inoremap l <Right>
" NORMAL move fast with Alt + hjkl
nnoremap l w
nnoremap h b
nnoremap j <C-d>
nnoremap k <C-u>
" VISUAL move fast with Alt + hjkl
vnoremap l w
vnoremap h b
vnoremap j <C-d>
vnoremap k <C-u>
" copy/past in/from "a register
nnoremap <Leader>y "ay
nnoremap <Leader>i "ayiw
vnoremap <Leader>y "ay
nnoremap <Leader>p "ap
nnoremap <Leader>P "aP
" search and replace
vnoremap <Leader>f <Esc>:%s/\%V
nnoremap <Leader>f :%s/
" quit Vim (fail if there is pending changes)
nnoremap <Leader>q :qall<CR>
" work inner by default (:h omap-info)
onoremap w iw
onoremap W iW
onoremap b ib
onoremap B iB
onoremap [ i[
onoremap < i<
onoremap t it
onoremap ' i'
onoremap " i"
onoremap ` i`
nnoremap é <Home>
vnoremap é <Home>
nnoremap " <End>
vnoremap " <End>
" hide highlight after a search
nnoremap <silent> <space> :nohlsearch<CR>
" c'est en forgeant que l'on devient forgeron
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Right> <Nop>
noremap <Left> <Nop>
" spell check
nnoremap <silent> <F2> :setlocal spell! spelllang=en_us<CR>
" open .vimrc, source it
nnoremap <F3> :tabnew $MYVIMRC<cr>
nnoremap <F4> :source $MYVIMRC<cr>

nnoremap <silent> <Leader>g :GitGutterToggle<CR>

" fzf
nnoremap <silent> <C-s> :Files<CR>
imap d <plug>(fzf-complete-path)
imap f <plug>(fzf-complete-file)
imap w <plug>(fzf-complete-line)

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
nnoremap <Leader>: :ALESymbolSearch
map <C-q> <Plug>(ale_hover)
nmap <silent> <C-PageUp> <Plug>(ale_previous_wrap)
nmap <silent> <C-PageDown> <Plug>(ale_next_wrap)

" tab
nnoremap <Leader>t :tabnew<CR>
noremap <silent> <C-Right> :tabn<CR>
noremap <silent> <C-Left> :tabp<CR>
nnoremap <silent> <C-Up> :+tabmove<CR>
nnoremap <silent> <C-Down> :-tabmove<CR>

" window
nnoremap <Leader>s :new<CR>
nnoremap <Leader>v :vnew<CR>
nnoremap <Leader><S-s> :split<CR>
nnoremap <Leader><S-v> :vsplit<CR>
nnoremap <silent> <C-h> <C-w>h
nnoremap <silent> <C-l> <C-w>l
nnoremap <silent> <C-k> <C-w>k
nnoremap <silent> <C-j> <C-w>j
nnoremap <silent> <Leader><Left> <C-w>H
nnoremap <silent> <Leader><Down> <C-w>J
nnoremap <silent> <Leader><Up> <C-w>K
nnoremap <silent> <Leader><Right> <C-w>L
noremap <Leader>= <C-w>=
nnoremap <silent> [1;3A :resize +4<CR>
nnoremap <silent> [1;3B :resize -4<CR>
nnoremap <silent> [1;3C :vertical :resize +4<CR>
nnoremap <silent> [1;3D :vertical :resize -4<CR>
" }}}

" fix gruvbox's highlight for Ale {{{
highlight ALEInfo ctermfg=109 cterm=italic
highlight ALEWarning ctermfg=214 cterm=italic
highlight ALEError ctermfg=167 cterm=italic
" fix spell highlight
highlight SpellBad ctermfg=167 cterm=underline
highlight SpellLocal ctermfg=108 cterm=underline
highlight SpellCap ctermfg=108 cterm=underline
highlight SpellRare ctermfg=108 cterm=underline
" change error highlight
highlight Error ctermfg=167 ctermbg=235 cterm=italic
highlight clear ErrorMsg
highlight ErrorMsg ctermfg=172 ctermbg=235
" }}}

" autocommand {{{
augroup stuff
  au!
  " remove all trailing white space before write
  autocmd BufWritePre * %s/\s\+$//e
  " Because Alt send escape sequence and there are mapping that use it,
  " Vim now waits 'timeoutlen' when escape is pressed before exit insert or
  " visual mode for example.
  " The following 2 lines are the trick to remove this delay. <nowait> works
  " only for buffer mapping. That's why we use BufEnter event to add this
  " mapping to a buffer each time we enter in a buffer.
  autocmd BufEnter * inoremap <buffer> <nowait> <Esc> <Esc>
  autocmd BufEnter * vnoremap <buffer> <nowait> <Esc> <Esc>
augroup END

" set fold to marker for .vimrc
augroup filetype_vim
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker
augroup END
" }}}
