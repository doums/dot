if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin(stdpath('data').'/plugged')

Plug 'scrooloose/nerdcommenter'
Plug 'airblade/vim-gitgutter'
Plug 'dense-analysis/ale'
Plug 'doums/barow'
Plug 'doums/coBra'
Plug 'doums/oterm'
Plug 'doums/nnnvi'
Plug 'doums/fzfTools'
Plug 'doums/darcula'
Plug 'doums/sae'
Plug 'doums/barowLSP'
Plug 'doums/barowGit'
Plug 'doums/rgv'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" web dev
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'jparise/vim-graphql'

call plug#end()

" vanilla config {{{
let mapleader = ","
set termguicolors
set relativenumber
set number
colorscheme darcula
set noshowmode
set shortmess=IFaWcs
set ignorecase
set smartcase
set cindent
set tabstop=2
set shiftwidth=2
set expandtab
set showmatch
set matchtime=3
set updatetime=100
set splitbelow
set splitright
set foldlevelstart=0
set textwidth=0
set hidden
set cursorline
set switchbuf=usetab
set scrolloff=5
set completeopt=menuone
set pumheight=10
set fillchars=vert:\ ,diff:\ ,fold:\ " a space
set complete=.,w,b,u,t,i
set clipboard+=unnamedplus
set guicursor=
set display=lastline
set spelllang=en_us
runtime ftplugin/man.vim
" }}}

" plugins config {{{

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

" nnnvi
let g:nnnvi = {
      \  'layout': { 'left': 40, 'min': 30 },
      \  'maps': {
      \    '<A-s>': 'split',
      \    '<A-v>': 'vsplit',
      \    '<A-a>': 'tabedit',
      \  }
      \}

" fzfTools
let g:fzfTools = {
      \  'gitlog': { 'tab': 1 },
      \  'gitlogsel': { 'tab': 1 },
      \}

" barow
let g:barow = {
      \  'modules': [
      \    [ 'barowGit#branch', 'BarowHint' ],
      \    [ 'barowLSP#error', 'BarowError' ],
      \    [ 'barowLSP#warning', 'BarowWarning' ],
      \    [ 'barowLSP#info', 'BarowInfo' ],
      \    [ 'barowLSP#hint', 'BarowHint' ],
      \    [ 'barowLSP#coc_status', 'Barow' ],
      \    [ 'barowLSP#ale_status', 'Barow' ]
      \  ]
      \}
hi! link StatusLine Barow
hi! link StatusLineNC BarowNC

" GitGutter
let g:gitgutter_enabled = 0
hi! link GitGutterAdd GitAddStripe
hi! link GitGutterChange GitChangeStripe
hi! link GitGutterDelete GitDeleteStripe
let g:gitgutter_sign_removed = '▶'

" typescript-vim
let g:typescript_indent_disable = 1

" ALE
let g:ale_disable_lsp = 1
let g:ale_sign_error = "▬"
let g:ale_sign_warning = "▬"
let g:ale_sign_info = "▬"
let g:ale_sign_style_error = "▬"
let g:ale_sign_style_warning = "▬"
let g:ale_set_highlights = 0
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_info_str = 'I'
let g:ale_echo_msg_format = '[%linter%][%severity%] %s'
let g:ale_fixers = {
      \ 'javascript': [ 'eslint', 'prettier' ],
      \ 'json': [ 'eslint' ],
      \ 'typescript': [ 'eslint', 'prettier' ],
      \ 'typescriptreact': [ 'eslint', 'prettier' ],
      \ 'graphql': [ 'eslint' ],
      \ 'rust': [ 'rustfmt' ]
      \ }
let g:ale_linters = {
      \ 'javascript': [ 'eslint' ],
      \ 'json': [ 'eslint' ],
      \ 'typescript': [ 'eslint', 'tsserver' ],
      \ 'typescriptreact': [ 'eslint', 'tsserver' ],
      \ 'graphql': [ 'eslint '],
      \ 'sh': [ 'shellcheck' ]
      \ }
let g:ale_linters_explicit = 1
let g:ale_fix_on_save = 1
let g:ale_completion_autoimport = 1
hi! link ALEError Error
hi! link ALEWarning CodeWarning
hi! link ALEInfo CodeInfo
hi! link ALEErrorSign ErrorSign
hi! link ALEWarningSign WarningSign
hi! link ALEInfoSign InfoSign

" coBra
let g:rust_keep_autopairs_default = 0
let g:coBraPairs = {
      \ 'rust': [
      \    ['<', '>'],
      \    ['"', '"'],
      \    ['{', '}'],
      \    ['(', ')'],
      \    ['[', ']']
      \ ]
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
" remap goto begin of line
noremap ù 0
" work inner by default
onoremap w iw
" copy in/past from "a register
noremap <Leader>y "ay
nnoremap <Leader>i "ayiw
nnoremap <Leader>p "ap
nnoremap <Leader>P "aP
" search and replace
vnoremap <Leader>f <Esc>:%s/\%V
nnoremap <Leader>f :%s/
" hide highlight after a search
nnoremap <silent> <space> :nohlsearch<CR>
" show trailing whitespaces
nnoremap <Leader><Space> /\s\+$<CR>
" tab
nnoremap <Leader>t :tabnew<CR>
noremap <silent> <C-Right> :tabn<CR>
noremap <silent> <C-Left> :tabp<CR>
nnoremap <silent> <C-Up> :+tabmove<CR>
nnoremap <silent> <C-Down> :-tabmove<CR>
nnoremap <Leader>) <C-w><C-]><C-w>T
" window
nnoremap <silent><Leader>s :new<CR>
nnoremap <silent><Leader>v :vnew<CR>
nnoremap <silent><Leader><S-s> :split<CR>
nnoremap <silent><Leader><S-v> :vsplit<CR>
nnoremap <silent> <A-h> <C-w>h
nnoremap <silent> <A-l> <C-w>l
nnoremap <silent> <A-k> <C-w>k
nnoremap <silent> <A-j> <C-w>j
noremap <Leader>= <C-w>=
nnoremap <silent> <A-Up> :resize +4<CR>
nnoremap <silent> <A-Down> :resize -4<CR>
nnoremap <silent> <A-Right> :vertical :resize +4<CR>
nnoremap <silent> <A-Left> :vertical :resize -4<CR>
" terminal mode
tnoremap <Leader>n <C-\><C-N>
" allow saving a file as sudo
cnoremap w!! :w :term sudo tee % > /dev/null
" toggle spell check
nnoremap <silent> <F2> :set spell!<cr>
inoremap <silent> <F2> <C-O>:set spell!<cr>
" open loclist
nmap <A-z> :lopen<CR>
" }}}

" {{{ plugins mapping

" GitGutter
nnoremap <silent> <Leader>g :GitGutterToggle<CR>
" oterm
nmap <Leader>o <Plug>OTerm
" fzfTools
nmap <C-s> <Plug>Ls
nmap <C-b> <Plug>Buf
nmap <C-g> <Plug>GitLog
vmap <C-g> <Plug>GitLogSel
" NERDCommenter
map <Leader>c <plug>NERDCommenterToggle
map <Leader><S-c> <plug>NERDCommenterSexy
" Ale
nmap <Leader>a <Plug>(ale_toggle)
nmap <Leader>b <Plug>(ale_go_to_definition)
nmap <Leader>n <Plug>(ale_go_to_type_definition)
nmap <Leader>r <Plug>(ale_find_references)
nmap <Leader>d <Plug>(ale_detail)
nnoremap <Leader>: :ALESymbolSearch
map <C-q> <Plug>(ale_hover)
nmap <A-e> <Plug>(ale_fix)
nmap <A-(> <Plug>(ale_previous_wrap)
nmap <A--> <Plug>(ale_next_wrap)
" nnnvi
nmap <Tab> <Plug>NNNs
nmap <S-Tab> <Plug>NNNnos
" rgv
nmap <A-o> <Plug>RgToggle
" }}}

" autocommand {{{
augroup stuff
autocmd!
" whenever CursorHold is fired (nothing typed during 'updatetime') in a normal
" bufer (&buftype option is empty) run checktime to refresh the buffer and
" retrieve any external changes
autocmd CursorHold * if empty(&buftype) | checktime % | endif
" set fold to marker for .vimrc
autocmd FileType vim setlocal foldmethod=marker
" set stuff for some programming languages
autocmd FileType * call s:CodeStuff()
autocmd FileType man set nonumber
" web dev, forces vim to rescan the entire buffer when highlighting for js and
" ts file
autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear
augroup END
" }}}

" darcula override {{{
hi! link rustQuestionMark PreProc
hi! link rustMacro PreProc
call darcula#Hi('rustLifetime', darcula#palette.macroName, darcula#palette.bg, 'italic')
call darcula#Hi('rustTypeParameter', darcula#palette.macroName, darcula#palette.bg, 'bold')
" }}}

" {{{ functions
function s:CodeStuff()
  if &filetype == "rust"
    nnoremap <buffer> <Leader>; iprintln!("{:#?}", );<Esc><Left>i
    inoremap <buffer> <Leader>; println!("{:#?}", );<Esc><Left>i
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

" {{{ Coc
" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
inoremap <silent><expr> <c-space> coc#refresh()

" Symbol renaming
nmap <A-r> <Plug>(coc-rename)

" Navigate diagnostics
nmap <silent> <A-"> <Plug>(coc-diagnostic-next)
nmap <silent> <A-'> <Plug>(coc-diagnostic-prev)

" GoTo code navigation.
nmap <silent> <A-b> <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)

" Find usages
nmap <silent> <A-u> <Plug>(coc-references)

" Apply AutoFix to problem on the current line.
nmap <A-CR> <Plug>(coc-fix-current)
nmap <A-S-a> <Plug>(coc-codeaction-line)

" Hover
nnoremap <silent> <A-d> :call <SID>show_documentation()<CR>

" Trigger completion
inoremap <silent><expr> <c-space> coc#refresh()

" Formatting selected code.
xmap <A-q> <Plug>(coc-format-selected)
nmap <A-q> <Plug>(coc-format-selected)

" Show all diagnostics.
nnoremap <silent> <A-a>  :<C-u>CocList diagnostics<CR>
" Find symbol of current document.
nnoremap <silent> <A-f>  :<C-u>CocList outline<CR>
" Search workspace symbols.
nnoremap <silent> <A-F>  :<C-u>CocList -I symbols<CR>

" Scroll floating window
nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<C-f>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<C-b>"

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" highlights
hi! link CocErrorSign ErrorSign
hi! link CocWarningSign WarningSign
hi! link CocInfoSign InfoSign
hi! link CocHintSign InfoSign
hi! link CocErrorFloat Pmenu
hi! link CocWarningFloat Pmenu
hi! link CocInfoFloat Pmenu
hi! link CocHintFloat Pmenu
hi! link CocHighlightText IdentifierUnderCaret
hi! link CocHighlightRead IdentifierUnderCaret
hi! link CocHighlightWrite IdentifierUnderCaretWrite
hi! link CocErrorHighlight CodeError
hi! link CocWarningHighlight CodeWarning
hi! link CocInfoHighlight CodeInfo
hi! link CocHintHighlight CodeHint
hi! link CocRustChainingHint CodeHint

augroup cocAutocmd
autocmd!
autocmd CursorHold * silent call CocActionAsync('highlight')
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

" restart rust-analyer
nnoremap <A-R> :CocCommand rust-analyzer.reload<CR>
" }}}

" {{{ scraps
" noremap <F9> :call <SID>DebugHi()<CR>
nnoremap <F5> :source $MYVIMRC<CR>

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
