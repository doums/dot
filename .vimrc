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
" NORMAL smooth scroll
nnoremap <silent> J :call <SID>ScrollDown()<CR>
nnoremap <silent> K :call <SID>ScrollUp()<CR>

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
nnoremap <F5> :write<Esc>:source $MYVIMRC<cr>

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

" script {{{

noremap cm :messages clear<CR>
noremap m : messages<CR>

let s:pairs = [
      \  ['"', '"'],
      \  ["'", "'"],
      \  ['`', '`'],
      \  ['{', '}'],
      \  ['(', ')'],
      \  ['[', ']']
      \]

for [open, close] in s:pairs
  if open != close
    execute 'inoremap <expr><silent> ' . open . ' <SID>AutoClose("' . escape(open, '"') . '", "' . escape(close, '"') . '")'
    execute 'inoremap <expr><silent> ' . close . ' <SID>SkipClose("' . escape(open, '"') . '", "' . escape(close, '"') . '")'
  else
    execute 'inoremap <expr><silent> ' . open . ' <SID>ManageQuote("' . escape(open, '"') . '")'
  endif
endfor

inoremap <expr> <BS> <SID>AutoDelete()

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
  if !s:IsEscaped()
        \ && !s:IsString(line("."), col("."))
        \ && !s:IsComment(line("."), col("."))
        \ && !s:IsBeforeOrInsideWord()
    return a:open.a:close."\<Left>"
  endif
  return a:open
endfunction

function s:SkipClose(open, close)
  if getline(".")[col(".") - 1] == a:close
        \ && s:SearchPair(a:open, a:close, 'cnW', 's:IsString(line("."), col(".")) || s:IsComment(line("."), col("."))') > 0
    return "\<Right>"
  endif
  return a:close
endfunction

function s:AutoDelete()
  for [open, close] in s:pairs
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
    let [line, col] = s:SearchPairPos(a:open, a:close, 'cnW')
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
    if s:InBetweenValid(a:close, start, end)
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
    else
      echom "FAIL"
    endif
  endif
endfunction

function s:InBetweenValid(close, start, end)
  if a:start.line == a:end.line
    return s:OneLineCheck(a:close, a:start, a:end)
  endif
  if match(getline(a:start.line), '^\s*$', a:start.col - 1) == -1
    echom "first line fail"
    return v:false
  endif
  echom 'first line OK'
  if a:start.line + 1 < a:end.line
    for row in getline(a:start.line + 1, a:end.line - 1)
      if match(row, '^\s*$') == -1
        echom "in between fail"
        return v:false
      endif
    endfor
    echom "in between lines OK"
  endif
  let lastLine = strpart(getline(a:end.line), 0, a:end.col)
  if match(lastLine, '^\s*'.escape(a:close, ']').'$') == -1
    echom "last line fail"
    return v:false
  endif
  echom "last line OK"
  return v:true
endfunction

function s:OneLineCheck(close, start, end)
  let line = strpart(getline(a:start.line), 0, a:end.col)
  if match(line, '^\s*'.escape(a:close, ']').'$', a:start.col - 1) == -1
    echom "one line fail"
    return v:false
  else
    echom "one line OK"
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

autocmd CursorMoved * call TestType()
autocmd CursorMovedI * call TestType()
function TestType()
  " echom synIDattr(synIDtrans(synID(line("."), col("."), 0)), "name")
  echo col(".") col("$")
endfunction

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
  for [open, close] in s:pairs
    if open != close
      let pattern = pattern.'\|'.escape(close, ']')
    endif
  endfor
  echom pattern
  if getline(".")[col(".") - 1] =~ pattern
    return v:false
  endif
  return v:true
endfunction

function s:SearchPair(open, close, flags, ...)
  if a:0 == 1
    return searchpair(escape(a:open, '['), '', escape(a:close, ']'), a:flags, a:1)
  else
    return searchpair(escape(a:open, '['), '', escape(a:close, ']'), a:flags)
  endif
endfunction

function s:SearchPairPos(open, close, flags, ...)
  if a:0 == 1
    return searchpairpos(escape(a:open, '['), '', escape(a:close, ']'), a:flags, a:1)
  else
    return searchpairpos(escape(a:open, '['), '', escape(a:close, ']'), a:flags)
  endif
endfunction

function s:GetSHL(line, col)
  return synIDattr(synIDtrans(synID(a:line, a:col, 0)), "name")
endfunction

  " }}}
