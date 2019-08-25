if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

let g:ale_completion_enabled=1

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
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
Plug 'doums/coBra'

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
filetype indent on
syntax on
" set mouse=a
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
set switchbuf=usetab
:runtime! ftplugin/man.vim
" }}}

" plugins config {{{
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
let g:fzf_buffers_jump = 1
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
" NORMAL smooth scroll
nnoremap <silent> J :call <SID>ScrollDown()<CR>
nnoremap <silent> K :call <SID>ScrollUp()<CR>
" NORMAL buffer
nnoremap <silent> n :bnext<CR>
nnoremap <silent> p :bprevious<CR>
nnoremap <silent> b :Buffers<CR>

function s:ScrollDown()
execute "normal!" &scroll / 2 . "\<C-e>"
endfunction

function s:ScrollUp()
execute "normal!" &scroll / 2 . "\<C-y>"
endfunction

" VISUAL move fast with Alt + hjkl
vnoremap l w
vnoremap h b
vnoremap j <C-d>
vnoremap k <C-u>
" NORMAL/VISUAL/OP_P move through wrapped line
noremap j gj
noremap k gk
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
" c'est en forgeant que l'on devient forgeron
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Right> <Nop>
noremap <Left> <Nop>
" spell check
nnoremap <silent> <F2> :setlocal spell! spelllang=en_us<CR>
" open .vimrc, source it
nnoremap <F3> :tabnew $MYVIMRC<CR>
" nnoremap <F5> :write<CR>:source $MYVIMRC<CR>:messages clear<CR>
noremap <F5> :write<CR>:Cargo run<CR>

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
nmap <Leader>b <Plug>(ale_go_to_definition)
nmap <Leader>n <Plug>(ale_go_to_type_definition)
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
autocmd!
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
" set fold to marker for .vimrc
autocmd FileType vim setlocal foldmethod=marker
" set a print shortcut for some programming languages
autocmd FileType * call s:PrintMaps()
augroup END
" }}}

function s:PrintMaps()
  if &filetype == "rust"
    nnoremap <buffer> <Leader>; iprintln!("")<Esc><Left>i
    inoremap <buffer> <Leader>; println!("")<Esc><Left>i
  endif
endfunction

finish
" coBra {{{

autocmd CursorMoved * call TestType()
autocmd CursorMovedI * call TestType()
function TestType()
  " echom synIDattr(synIDtrans(synID(line("."), col("."), 0)), "name")
  echo line(".") col(".")
endfunction

noremap rm :messages clear<CR>
noremap m :messages<CR>

" let s:save_cpo = &cpo
" set cpo&vim

" if exists("g:coBra")
  " finish
" endif
" let g:coBra = 1
let g:defaultPairs = [
      \  ['"', '"'],
      \  ["'", "'"],
      \  ['`', '`'],
      \  ['{', '}'],
      \  ['(', ')'],
      \  ['<', '>'],
      \  ['[', ']']
      \ ]
let b:pairs = g:defaultPairs

if !exists('g:coBraPairs')
  let g:coBraPairs = { 'default': g:defaultPairs }
elseif !has_key(g:coBraPairs, 'default')
  let g:coBraPairs.default = g:defaultPairs
endif

" if !exists("g:coBraMaxPendingCloseTry")
  " let g:coBraMaxPendingCloseTry = 10
" endif

let s:recursiveCount = 0
let g:coBraLineMax = 10
let g:coBraMaxPendingCloseTry = 10

augroup coBra
  autocmd!
  autocmd FileType * call s:Init()
augroup END

function s:Init()
  for type in keys(g:coBraPairs)
    if type == &filetype
      return s:setPairsAndMap(type)
    endif
  endfor
  return s:setPairsAndMap('default')
endfunction

function s:setPairsAndMap(type)
  let b:pairs = g:coBraPairs[a:type]
  for [open, close] in b:pairs
    if open != close
      execute 'inoremap <buffer><expr><silent> '.open.
            \' <SID>AutoClose("'.escape(open, '"').'", "'.escape(close, '"').'")'
      execute 'inoremap <buffer><expr><silent> '.close.
            \' <SID>SkipClose("'.escape(open, '"').'", "'.escape(close, '"').'")'
    else
      execute 'inoremap <buffer><expr><silent> '.open.' <SID>ManageQuote("'.escape(open, '"').'")'
    endif
  endfor
  inoremap <buffer><expr> <BS> <SID>AutoDelete()
  inoremap <buffer><expr> <CR> <SID>AutoBreak()
endfunction

function s:ManageQuote(quote)
  if s:IsString(line("."), col("."))
        \ && s:IsString(line("."), col(".") - 1)
        \ && getline(".")[col(".") - 1] == a:quote
        \ && !s:IsEscaped()
    return "\<Right>"
  endif
  return s:AutoClose(a:quote, a:quote)
endfunction

function s:AutoClose(open, close)
  if s:IsEscaped()
        \ || s:IsString(line("."), col("."))
        \ || s:IsComment(line("."), col("."))
        \ || s:IsPendingClose(a:open, a:close)
    return a:open
  endif
  if !s:IsBeforeOrInsideWord()
    return a:open.a:close."\<Left>"
  endif
  return a:open
endfunction

function s:SkipClose(open, close)
  if getline(".")[col(".") - 1] == a:close
        \ && searchpair(escape(a:open, '['),
        \ '',
        \ escape(a:close, ']'),
        \ 'cnW',
        \ 's:IsString(line("."), col(".")) || s:IsComment(line("."), col("."))',
        \ s:GetLineBoundary('f')) > 0
    return "\<Right>"
  endif
  return a:close
endfunction

" auto break {{{
function s:AutoBreak()
  for [open, close] in b:pairs
    if open != close && getline(line("."))[col(".") - 2] == open
      let [line, col] = searchpairpos(escape(open, '['),
            \ '',
            \ escape(close, ']'),
            \ 'cnW',
            \ 's:IsString(line("."), col(".")) || s:IsComment(line("."), col("."))',
            \ s:GetLineBoundary('f'))
      if line == line(".") &&
            \ match(getline("."), '^'.escape(open, '[').'\s*'.escape(close, ']'), col(".") - 2) > -1
        return "\<CR>\<CR>\<Up>\<C-f>"
      endif
    endif
  endfor
  return "\<CR>"
endfunction
" }}}

" pending close {{{
function s:IsPendingClose(open, close)
  if a:open == a:close
    return
  endif
  let currentLine = line(".")
  let currentCol = col(".")
  let s:recursiveCount = 0
  if s:UnderCursorSearch(a:open, a:close, s:GetLineBoundary('b'))
    return v:true
  endif
  let result = s:RecursiveSearch(a:open, a:close, s:GetLineBoundary('f'), s:GetLineBoundary('b'))
  call cursor(currentLine, currentCol)
  return result
endfunction

function s:UnderCursorSearch(open, close, stopLine)
  if getline(".")[col(".") - 1] == a:close && !s:IsArrowOrGreaterLessSign(a:close, line("."), col(".") - 1)
    let [line, col] = searchpairpos(escape(a:open, '['),
          \ '',
          \ escape(a:close, ']'),
          \ 'bWn',
          \ 's:IsString(line("."), col(".")) || s:IsComment(line("."), col("."))',
          \ a:stopLine)
    if line == 0 && col == 0
      echom "free close bracket found under cursor"
      return v:true
    endif
  endif
endfunction

function s:RecursiveSearch(open, close, maxForward, maxBackward)
  echom "cursor pos ".line(".")
  if s:recursiveCount >= &maxfuncdepth - 10 || s:recursiveCount >= g:coBraMaxPendingCloseTry
    echom "max recursive depth reached"
    return
  endif
  let s:recursiveCount = s:recursiveCount + 1
  let [line, col] = searchpos(escape(a:close, ']'), 'eWz', a:maxForward)
  if line == 0 && col == 0
    return
  endif
  if s:IsString(line, col) ||
        \ s:IsComment(line, col) ||
        \ s:IsArrowOrGreaterLessSign(a:close, line, col - 1)
    return s:RecursiveSearch(a:open, a:close, a:maxForward, a:maxBackward)
  endif
  echom "one close found at ".line.' '.col
  let [pairLine, pairCol] = searchpairpos(escape(a:open, '['),
        \ '',
        \ escape(a:close, ']'),
        \ 'bnW',
        \ 's:IsString(line("."), col(".")) || s:IsComment(line("."), col(".")) || s:IsArrowOrGreaterLessSign(a:open, line("."), col(".") - 1)',
        \ a:maxBackward)
  echom "found a match at ".pairLine.' '.pairCol
  if pairLine == 0 && pairCol == 0
    echom "found a free close bracket"
    return v:true
  endif
  return s:RecursiveSearch(a:open, a:close, a:maxForward, a:maxBackward)
endfunction
" }}}

" auto delete {{{
function s:AutoDelete()
  for [open, close] in b:pairs
    if open == close
      let result = s:DeleteQuotes(open)
      if !empty(result)
        return result
      endif
    else
      let result = s:DeletePair(open, close)
      if !empty(result)
        return result
      endif
    endif
  endfor
  return "\<BS>"
endfunction

function s:DeletePair(open, close)
  if getline(".")[col(".") - 2] == a:open
        \ && getline(".")[col(".") - 3] != '\'
    let [line, col] = searchpairpos(escape(a:open, '['),
          \ '',
          \ escape(a:close, ']'),
          \ 'cnW',
          \ '',
          \ s:GetLineBoundary('f'))
    if line == 0 && col == 0
      return
    endif
    let start = {'line': line("."), 'col': col(".")}
    let end = {'line': line, 'col': col}
    echom "start ".string(start)
    echom "end ".string(end)
    if start.line == end.line && end.col == start.col
      return "\<Del>\<BS>"
    endif
    if s:IsPairEmpty(a:open, a:close, start, end)
      if start.line == end.line
        let toEnd = end.col - 2
        let toStart = ''
        if start.col - 2 > 0
          let toStart = (start.col - 2).'l'
        endif
        return "\<BS>\<Esc>0".toEnd.'lx0'.toStart.'i'
      else
        let toEnd = ''
        let toStart = ''
        let insertMotion = ''
        if end.col - 1 > 0
          let toEnd = (end.col - 1).'l'
        endif
        let i = 0
        if start.col == col("$")
          let i = start.col - 3
          if col("$") - 1 > 1
            let insertMotion = "\<Right>"
          endif
        else
          let i = start.col - 2
        endif
        if i > 0
          let toStart = i.'l'
        endif
        return "\<BS>\<Esc>".end.line.'G0'.toEnd.'x'.start.line.'G0'.toStart.'i'.insertMotion
      endif
    endif
  endif
endfunction

function s:IsPairEmpty(open, close, start, end)
  if a:start.line == a:end.line
    let [line, col] = searchpos(escape(a:open, '[').'\s*'.escape(a:close, ']'), 'bW', a:start.line)
  else
    let [line, col] = searchpos(escape(a:open, '[').'\(\s*\n\)\{'.(a:end.line - a:start.line).'}\s*'.escape(a:close, ']'), 'bW', a:start.line)
  endif
  if line == a:start.line && col == a:start.col - 1
    return v:true
  endif
endfunction

function s:DeleteQuotes(quote)
  if getline(".")[col(".") - 1] == a:quote
        \ && getline(".")[col(".") - 2] == a:quote
        \ && !s:IsComment(line("."), col(".") - 1)
        \ && !s:IsString(line("."), col(".") + 1)
        \ && !s:IsString(line("."), col(".") - 2)
        \ && getline(".")[col(".") - 3] != '\'
    return "\<Del>\<BS>"
  endif
endfunction
" }}}

" helpers {{{
function s:IsString(line, col)
  if s:GetSHL(a:line, a:col) =~? "string"
    return v:true
  endif
endfunction

function s:IsComment(line, col)
  if s:GetSHL(a:line, a:col) =~? "comment"
    return v:true
  endif
  if col(".") == col("$")
        \ && !getline(line("."))[col(".") - 1]
        \ && s:GetSHL(a:line, col(".") - 1) =~? "comment"
    return v:true
  endif
endfunction

function s:IsEscaped()
  if getline(".")[col(".") - 2] == '\'
    return v:true
  endif
endfunction

function s:IsBeforeOrInsideWord()
  if col(".") == col("$")
    return v:false
  endif
  let pattern = '\s'
  for [open, close] in b:pairs
    if open != close
      let pattern = pattern.'\|'.escape(close, ']')
    endif
  endfor
  if getline(".")[col(".") - 1] =~ pattern.'\|[,;]'
    return v:false
  endif
  return v:true
endfunction

function s:GetLineBoundary(direction)
  if (!exists("g:coBraLineMax") || g:coBraLineMax) <= 0 && !exists("g:coBraFullWindow")
    if a:direction == 'f'
      echom "line boundary is ".line("w$")
      return line("w$")
    else
      echom "line boundary is ".line("w0")
      return line("w0")
    endif
  endif
  if a:direction == 'f'
    let boundary = line(".") + g:coBraLineMax - 1
    if boundary > line("$")
      echom "line boundary is ".line("$")
      return line("$")
    else
      echom "line boundary is ".boundary
      return boundary
    endif
  endif
  if a:direction == 'b'
    let boundary = line(".") - g:coBraLineMax + 1
    if boundary < 1
      echom "line boundary is 1"
      return 1
    else
      echom "line boundary is ".boundary
      return boundary
    endif
  endif
endfunction

function s:IsArrowOrGreaterLessSign(c, lnum, index)
  if a:c != '<' && a:c != '>'
    return
  endif
  let line = getline(a:lnum)
  if strpart(line, a:index - 1, 2)  =~ '^[-=]>$'
    echom "arrow"
    return v:true
  elseif strpart(line, a:index, 2)  =~ '^[<>]=$'
    echom "greater/less or equal"
    return v:true
  elseif match(line, '^\s\=[<>]\s\=\(-\=\d\|\w\)', a:index - 1) > -1
    echom "greater/less"
    return v:true
  endif
endfunction

function s:GetSHL(line, col)
  return synIDattr(synIDtrans(synID(a:line, a:col, 0)), "name")
endfunction
" }}}

" let &cpo = s:save_cpo
" unlet s:save_cpo

" }}}


