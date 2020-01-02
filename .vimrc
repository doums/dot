if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf.vim'
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
Plug 'doums/darcula'
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
:runtime! ftplugin/man.vim
" }}}

" plugins config {{{
let g:deoplete#enable_at_startup = 1
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_winsize = 30
let g:NERDCreateDefaultMappings = 0
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
let g:lightline = {
      \ 'colorscheme': 'darculaOriginal',
			\ 'component_function': {
			\   'gitbranch': 'fugitive#head'
			\ }
      \ }
let g:lightline.active = {
      \ 'right': [
      \   [ 'lineinfo' ],
      \   [ 'percent' ],
      \   [ 'gitbranch', 'fileformat', 'fileencoding', 'filetype' ]
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
let g:gitgutter_enabled = 0
let g:typescript_indent_disable = 1
let g:ale_set_highlights = 0
" Use ALE's function for omnicompletion, :h omnifunc
" set omnifunc=ale#completion#OmniFunc
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_info_str = 'I'
let g:ale_echo_msg_format = '[%linter%][%severity%] %s'
let g:ale_fixers = {
      \ 'javascript': [ 'eslint' ],
      \ 'typescript': [ 'eslint', 'tsserver' ],
      \ 'graphql': [ 'eslint' ],
      \ 'rust': [ 'rustfmt' ]
      \ }
let g:ale_linters = {
      \ 'javascript': [ 'eslint', 'standard' ],
      \ 'typescript': [ 'eslint', 'tsserver' ],
      \ 'graphql': [ 'eslint '],
      \ 'rust': [ 'cargo', 'rls', 'rustc', 'clippy', 'rustfmt' ]
      \ }
let g:ale_linters_explicit = 1
let g:ale_fix_on_save = 1
let g:fzf_colors = {
      \ 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Function'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'PreProc'],
      \ 'header':  ['fg', 'Comment']
      \ }
let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit'
      \ }
let g:fzf_buffers_jump = 1
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
" NORMAL/VISUAL/OP_P move through wrapped line
noremap <silent> j gj
noremap <silent> k gk
" remap goto begin and end of line
noremap H 0
noremap L $
" NORMAL smooth scroll
nnoremap <silent> J :call <SID>ScrollDown()<CR>
nnoremap <silent> K :call <SID>ScrollUp()<CR>
" NORMAL buffer
nnoremap <silent> n :bnext<CR>
nnoremap <silent> p :bprevious<CR>
nnoremap <silent> b :Buffers<CR>
" copy in/past from "a register
noremap <Leader>y "ay
nnoremap <Leader>i "ayiw
nnoremap <Leader>p "ap
nnoremap <Leader>P "aP
" search and replace
vnoremap <Leader>f <Esc>:%s/\%V
nnoremap <Leader>f :%s/
" quit Vim (fail if there is pending changes)
nnoremap <Leader>q :qall<CR>
" OP_P work inner by default (:h omap-info)
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
" spell check
nnoremap <silent> <F2> :setlocal spell! spelllang=en_us<CR>
" open .vimrc
nnoremap <F3> :tabnew $MYVIMRC<CR>
" replace the word under the cursor
" by the first or the selected completion suggestion
inoremap <expr> <Tab> <SID>Complete()
" }}}

" {{{ plugins mapping

" GitGutter
nnoremap <silent> <Leader>g :GitGutterToggle<CR>
" fzf
nnoremap <silent> <C-s> :Files<CR>
imap d <plug>(fzf-complete-path)
imap f <plug>(fzf-complete-file)
imap w <plug>(fzf-complete-line)
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
nmap <silent> <C-PageUp> <Plug>(ale_previous_wrap)
nmap <silent> <C-PageDown> <Plug>(ale_next_wrap)
" }}}

" autocommand {{{
augroup stuff
autocmd!
" Because Alt send escape sequence and there are mapping that use it,
" Vim now waits 'timeoutlen' when escape is pressed before exit insert or
" visual mode for example.
" The following 2 lines are the trick to remove this delay. <nowait> works
" only for buffer mapping. That's why we use BufEnter event to add this
" mapping to a buffer each time we enter in a buffer.
autocmd BufEnter * inoremap <buffer> <nowait> <Esc> <Esc>
autocmd BufEnter * vnoremap <buffer> <nowait> <Esc> <Esc>
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

function s:ScrollDown()
  execute "normal!" &scroll / 2 . "\<C-e>"
endfunction

function s:ScrollUp()
  execute "normal!" &scroll / 2 . "\<C-y>"
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

" {{{ scraps
" noremap <F9> :call <SID>DebugHi()<CR>
" nnoremap <F5> :source $MYVIMRC<CR>

" autocmd CursorMoved * call s:DebugHi()

function s:DebugHi()
  let name = synID(line("."), col("."), 1)->synIDattr("name")
  let link= synID(line("."), col("."), 1)->synIDtrans()->synIDattr("name")
  let fg = synID(line("."), col("."), 1)->synIDtrans()->synIDattr("fg")
  let bg = synID(line("."), col("."), 1)->synIDtrans()->synIDattr("bg")
  echo 'hi: '.name.', link: '.link.', bg: '.bg.', fg: '.fg
endfunction
" }}}
