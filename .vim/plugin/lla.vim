if exists("g:lla")
  finish
endif
let g:lla = 1

function lla#Errors()
  if !s:ALERuns()
    return ''
  endif
  let stats = ale#statusline#Count(bufnr("%"))
  let errors = stats.error + stats.style_error
  if errors > 0
    return errors
  else
    return ''
  endif
endfunction

function lla#Warnings()
  if !s:ALERuns()
    return ''
  endif
  let stats = ale#statusline#Count(bufnr("%"))
  let warnings = stats.warning + stats.style_warning
  if warnings > 0
    return warnings
  else
    return ''
  endif
endfunction

function lla#Ok()
  if !s:ALERuns()
    return ''
  endif
  let stats = ale#statusline#Count(bufnr("%"))
  let errors = stats.error + stats.style_error
  let warnings = stats.warning + stats.style_warning
  if errors == 0 && warnings == 0
    return 'ok'
  else
    return ''
  endif
endfunction

function lla#Checking()
  return ale#engine#IsCheckingBuffer(bufnr("%")) ? '..' : ''
endfunction

function s:ALERuns()
  return get(g:, 'ale_enabled', 0) == 1
    \ && getbufvar(bufnr("%"), 'ale_linted', 0) > 0
    \ && ale#engine#IsCheckingBuffer(bufnr("%")) == 0
endfunction

augroup lla
  autocmd!
  autocmd User ALEJobStarted call lightline#update()
  autocmd User ALELintPost call lightline#update()
  autocmd User ALEFixPost call lightline#update()
augroup END
