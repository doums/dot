-- ALIASES -------------------------------------------------------
local fn = vim.fn
local cmd = vim.cmd
local g = vim.g

-- PLUGINS -------------------------------------------------------
-- auto install paq-nvim
local install_path = fn.stdpath('data')..'/site/pack/paqs/opt/paq-nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  cmd('!git clone https://github.com/savq/paq-nvim.git '..install_path)
end

cmd 'packadd paq-nvim'             -- Load package
local paq = require'paq-nvim'.paq  -- Import module and bind `paq` function

-- update treesitter parsers
local function update_ts_parsers() cmd 'TSUpdate' end

paq {'savq/paq-nvim', opt=true}    -- Let Paq manage itself
paq 'b3nj5m1n/kommentary'
paq 'airblade/vim-gitgutter'
paq 'dense-analysis/ale'
paq 'doums/barow'
paq 'doums/coBra'
paq 'doums/oterm'
paq 'doums/nnnvi'
paq 'doums/fzfTools'
paq 'doums/darcula'
paq 'doums/sae'
paq 'doums/barowLSP'
paq 'doums/barowGit'
paq 'doums/rgv'
paq 'doums/lspfzf'
paq {'nvim-treesitter/nvim-treesitter', run=update_ts_parsers}
paq 'nvim-treesitter/playground'
paq 'neovim/nvim-lspconfig'
paq 'hrsh7th/nvim-compe'
paq 'nvim-lua/lsp_extensions.nvim'

-- HELPERS -------------------------------------------------------
--[[ make buffer and window option global as well
     TODO use vim.opt when released
     see https://github.com/neovim/neovim/pull/13479 ]]
local function opt(key, value, scope)
  scope = scope or 'o'
  local scopes = {o = vim.o, b = vim.bo, w = vim.wo}
  scopes[scope][key] = value
  if scope ~= 'o' then scopes['o'][key] = value end
end

-- `t` for `termcodes`.
local function t(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

-- map with `noremap` option set to `true` by default
local function map(mode, lhs, rhs, opts)
  opts = opts or {noremap = true}
  if opts.noremap == nil then opts.noremap = true end
  if opts.buffer then
    opts.buffer = nil
    vim.api.nvim_buf_set_keymap(0, mode, lhs, rhs, opts)
  else
    opts.buffer = nil
    vim.api.nvim_set_keymap(mode, lhs, rhs, opts)
  end
end

-- OPTIONS -------------------------------------------------------
opt('termguicolors', true)
opt('number', true, 'w')
opt('relativenumber', true, 'w')
opt('showmode', false)
opt('shortmess', 'IFaWcs')
opt('ignorecase', true)
opt('smartcase', true)
opt('cindent', true, 'b')
opt('tabstop', 2, 'b')
opt('shiftwidth', 2, 'b')
opt('expandtab', true, 'b')
opt('showmatch', true)
opt('matchtime', 3)
opt('updatetime', 100)
opt('splitbelow', true)
opt('splitbelow', true)
opt('splitright', true)
opt('foldlevelstart', 0)
opt('hidden', true)
opt('cursorline', true, 'w')
opt('switchbuf', 'usetab')
opt('scrolloff', 5, 'w')
opt('completeopt', 'menuone,noselect')
opt('pumheight', 10)
opt('fillchars', 'vert: ,diff: ,fold: ', 'w')
opt('complete', vim.bo.complete..',i', 'b')
opt('clipboard', 'unnamedplus')
opt('guicursor', '')

-- VARIOUS -------------------------------------------------------
-- color scheme
cmd 'colorscheme darcula'
-- nvim as man pager
cmd 'runtime ftplugin/man.vim'
-- map leader
g.mapleader = ','

-- MAPPINGS ------------------------------------------------------
-- c'est en forgeant que l'on devient forgeron
map('', '<Up>', '<Nop>')
map('', '<Down>', '<Nop>')
map('', '<Right>', '<Nop>')
map('', '<Left>', '<Nop>')
-- move fast with Ctrl + hjkl
map('', '<C-l>', '<Plug>SaeRight', {noremap=false})
map('', '<C-h>', '<Plug>SaeLeft', {noremap=false})
map('', '<C-j>', '<C-d>')
map('', '<C-k>', '<C-u>')
-- move through wrapped line
map('', 'j', 'gj', {silent=true})
map('', 'k', 'gk', {silent=true})
-- goto start and end of line
map('', '<space>l', '$')
map('', '<space>h', '0')
-- work inner by default
map('o', 'w', 'iw')
-- search and replace
map('v', '<Leader>f', '<Esc>:%s/\\%V')
map('n', '<Leader>f', ':%s/')
-- hide highlight after a search
map('n', '<space>', ':nohlsearch<CR>', {silent=true})
-- show trailing whitespaces
map('n', '<Leader><Space>', '/\\s\\+$<CR>')
-- tabs
map('n', '<Leader>t', ':tabnew<CR>')
map('', '<C-Right>', ':tabn<CR>', {silent=true})
map('', '<C-Left>', ':tabp<CR>', {silent=true})
map('', '<C-Up>', ':+tabmove<CR>', {silent=true})
map('', '<C-Down>', ':-tabmove<CR>', {silent=true})
-- windows
map('n', '<Leader>s', ':new<CR>', {silent=true})
map('n', '<Leader>v', ':vnew<CR>', {silent=true})
map('n', '<Leader><S-s>', ':split<CR>', {silent=true})
map('n', '<Leader><S-v>', ':vsplit<CR>', {silent=true})
map('n', '<A-h>', '<C-w>h', {silent=true})
map('n', '<A-l>', '<C-w>l', {silent=true})
map('n', '<A-j>', '<C-w>j', {silent=true})
map('n', '<A-k>', '<C-w>k', {silent=true})
map('n', '<A-Up>', ':resize +4<CR>', {silent=true})
map('n', '<A-Down>', ':resize -4<CR>', {silent=true})
map('n', '<A-Right>', ':vertical resize +4<CR>', {silent=true})
map('n', '<A-Left>', ':vertical resize -4<CR>', {silent=true})
-- terminal normal mode
map('t', '<Leader>n', '<C-\\><C-N>')

-- AUTOCOMMANDS --------------------------------------------------
-- see https://github.com/neovim/neovim/pull/12378
vim.api.nvim_exec([[
  augroup init.lua
    autocmd!
    " whenever CursorHold is fired (nothing typed during 'updatetime') in a normal
    " bufer (&buftype option is empty) run checktime to refresh the buffer and
    " retrieve any external changes
    autocmd CursorHold * if empty(&buftype) | checktime % | endif
    " set stuff for some programming languages
    autocmd FileType * call v:lua.code_log()
    autocmd FileType man set nonumber
  augroup END
]], false)

-- FUNCTIONS -----------------------------------------------------
function _G.code_log()
  if vim.bo.filetype == 'rust' then
    map('n', '<Leader>;', 'iprintln!("{:#?}", );<Esc><Left>i', {buffer=true})
    map('i', '<Leader>;', 'println!("{:#?}", );<Esc><Left>i', {buffer=true})
  end
  if vim.bo.filetype == 'javascript' or vim.bo.filetype == 'typescript' then
    map('n', '<Leader>;', "iconsole.log('')<Esc><Left>i", {buffer=true})
    map('i', '<Leader>;', "console.log('')<Esc><Left>i", {buffer=true})
  end
end

-- barow ---------------------------------------------------------
g.barow = {
  modules = {
    {'barowGit#branch', 'BarowHint'},
    {'barowLSP#error', 'BarowError'},
    {'barowLSP#warning', 'BarowWarning'},
    {'barowLSP#info', 'BarowInfo'},
    {'barowLSP#hint', 'BarowHint'},
    {'barowLSP#coc_status', 'Barow'},
    {'barowLSP#ale_status', 'Barow'}
  }
}
cmd 'hi! link StatusLine Barow'
cmd 'hi! link StatusLineNC BarowNC'

-- kommentary ----------------------------------------------------
g.kommentary_create_default_mappings = false
map('n', '<leader>cc', '<Plug>kommentary_line_default', {noremap=false})
map('n', '<leader>c', '<Plug>kommentary_motion_default', {noremap=false})
map('v', '<leader>c', '<Plug>kommentary_visual_default', {noremap=false})

-- coBra ---------------------------------------------------------
g.coBraPairs = {
  rust = {
    {'<', '>'},
    {'"', '"'},
    {'{', '}'},
    {'(', ')'},
    {'[', ']'}
  }
}

-- OTerm ---------------------------------------------------------
map('n', '<Leader>o', '<Plug>OTerm', {noremap=false})

-- fzfTools ------------------------------------------------------
g.fzfTools = {
  gitlog = {tab = 1},
  gitlogsel = {tab = 1},
}
map('n', '<C-s>', '<Plug>Ls', {noremap=false})
map('n', '<C-b>', '<Plug>Buffers', {noremap=false})
map('n', '<A-p>', '<Plug>Registers', {noremap=false})
map('n', '<C-g>', '<Plug>GitLog', {noremap=false})
map('n', '<C-g>', '<Plug>SGitLog', {noremap=false})

-- nnnvi ---------------------------------------------------------
g.nnnvi = {
  layout = {left = 40, min = 30},
  maps = {
    ['<A-s>'] = 'split',
    ['<A-v>'] = 'vsplit',
    ['<A-a>'] = 'tabedit',
  }
}
map('n', '<Tab>', '<Plug>NNNs', {noremap=false})
map('n', '<S-Tab>', '<Plug>NNNnos', {noremap=false})

-- rgv -----------------------------------------------------------
map('n', '<A-o>', '<Plug>RgToggle', {noremap=false})

-- nvim-treesitter -----------------------------------------------
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true
  },
  indent = {
    enable = true
  },
  playground = {
    enable = true,
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false -- Whether the query persists across vim sessions
  },
}

-- LSP -----------------------------------------------------------
local lspconfig = require'lspconfig'

fn.sign_define('LspDiagnosticsSignError', {text = '▬', texthl = 'LspDiagnosticsSignError'})
fn.sign_define('LspDiagnosticsSignWarning', {text = '▬', texthl = 'LspDiagnosticsSignWarning'})
fn.sign_define('LspDiagnosticsSignInformation', {text = '▬', texthl = 'LspDiagnosticsSignInformation'})
fn.sign_define('LspDiagnosticsSignHint', {text = '▬', texthl = 'LspDiagnosticsSignHint'})

map('n', '<A-a>', '<cmd>lua vim.lsp.diagnostic.goto_prev{popup_opts={show_header=false}}<CR>')
map('n', '<A-z>', '<cmd>lua vim.lsp.diagnostic.goto_next{popup_opts={show_header=false}}<CR>')
map('n', '<A-CR>', '<cmd>lua vim.lsp.buf.code_action()<CR>')
map('v', '<A-CR>', '<cmd>lua vim.lsp.buf.range_code_action()<CR>')
map('n', '<A-b>', '<cmd>lua vim.lsp.buf.definition()<CR>')
map('n', '<A-t>', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
map('n', '<A-d>', '<cmd>lua vim.lsp.buf.hover()<CR>')
map('n', '<A-r>', '<cmd>lua vim.lsp.buf.rename()<CR>')
map('n', '<A-u>', '<cmd>lua vim.lsp.buf.references()<CR>')
map('n', '<A-s>', '<cmd>lua vim.lsp.buf.document_symbol()<CR>')
map('n', '<A-g>', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
map('n', '<A-w>', '<cmd>lua vim.lsp.buf.workspace_symbol("")<CR>')

local function on_attach(client)
  if client.resolved_capabilities.document_range_formatting then
    map('n', '<A-f>', '<cmd>lua vim.lsp.buf.range_formatting()<CR>')
  elseif client.resolved_capabilities.document_formatting then
    map('n', '<A-f>', '<cmd>lua vim.lsp.buf.formatting()<CR>')
  end
  -- open a floating window with the diagnostics from the current cursor position
  vim.api.nvim_exec([[
    augroup lsp_on_attach
      autocmd!
      autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics{show_header=false}
    augroup END
  ]], false)
  -- highlight the symbol under the cursor
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
      augroup lsp_document_highlight
        autocmd!
        autocmd CursorHold,CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end
end

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = false
  }
)

require'lspconfig'.clangd.setup {on_attach = on_attach}    -- C, C++
require'lspconfig'.tsserver.setup {on_attach = on_attach}  -- TypeScript
lspconfig.rust_analyzer.setup {                            -- Rust
  on_attach = on_attach,
  settings = {
    ['rust-analyzer.checkOnSave.command'] = 'clippy'
  }
}
require'lspconfig'.sumneko_lua.setup {                     -- Lua
  cmd = {'/opt/lua-language-server/bin/Linux/lua-language-server', '-E', '/opt/lua-language-server/main.lua'},
  settings = {
    Lua = {
      runtime = {
        -- LuaJIT for Neovim
        version = 'LuaJIT',
        path = vim.split(package.path, ';'),
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
      -- Do not send telemetry data
      telemetry = {
        enable = false,
      }
    }
  }
}

-- lspfzf --------------------------------------------------------
require'lspfzf'.setup {}

-- lsp_extensions.nvim -------------------------------------------
-- enable inlay hints for Rust
vim.api.nvim_exec([[
  augroup lsp_inlay_hints
    autocmd!
    autocmd CursorHold,BufEnter,BufWinEnter,TabEnter,BufWritePost *.rs :lua require'lsp_extensions'.inlay_hints{highlight="CodeHint", prefix=""}
  augroup END
]], false)

-- nvim-compe ----------------------------------------------------
require'compe'.setup {
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = 'enable',
  throttle_time = 80,
  source_timeout = 200,
  incomplete_delay = 400,
  max_abbr_width = 100,
  max_kind_width = 100,
  max_menu_width = 100,
  documentation = true,
  source = {
    path = true,
    buffer = true,
    calc = true,
    nvim_lsp = true,
    nvim_lua = true
  }
}

local function check_back_space()
  local col = fn.col('.') - 1
  if col == 0 or fn.getline('.'):sub(col, col):match('%s') then
    return true
  else
    return false
  end
end

function _G.tab_complete()
  if fn.pumvisible() == 1 then
    return t'<C-n>'
  elseif check_back_space() then
    return t'<Tab>'
  else
    return fn['compe#complete']()
  end
end

function _G.s_tab_complete()
  if fn.pumvisible() == 1 then
    return t'<C-p>'
  else
    return t'<S-Tab>'
  end
end

map('i', '<Tab>', 'v:lua.tab_complete()', {expr=true})
map('s', '<Tab>', 'v:lua.tab_complete()', {expr=true})
map('i', '<S-Tab>', 'v:lua.s_tab_complete()', {expr=true})
map('s', '<S-Tab>', 'v:lua.s_tab_complete()', {expr=true})
map('i', '<C-Space>', 'compe#complete()', {silent=true, expr=true})
map('i', '<CR>', "compe#confirm('<CR>')", {silent=true, expr=true})
map('i', '<C-e>', "compe#close('<C-e>')", {silent=true, expr=true})

-- ALE -----------------------------------------------------------
g.ale_disable_lsp = 1
g.ale_sign_error = '▬'
g.ale_sign_warning = '▬'
g.ale_sign_info = '▬'
g.ale_sign_style_error = '▬'
g.ale_sign_style_warning = '▬'
g.ale_set_highlights = 0
g.ale_echo_msg_error_str = 'E'
g.ale_echo_msg_warning_str = 'W'
g.ale_echo_msg_info_str = 'I'
g.ale_echo_msg_format = '[%linter%][%severity%] %s'
g.ale_linters_explicit = 1
g.ale_fix_on_save = 1
g.ale_completion_autoimport = 1
g.ale_fixers = {
  javascript = {'eslint', 'prettier'},
  json = {'eslint'},
  typescript = {'eslint', 'prettier'},
  typescriptreact = {'eslint', 'prettier'},
  graphql = {'eslint'},
  rust = {'rustfmt'}
}
g.ale_linters = {
  javascript = {'eslint'},
  json = {'eslint'},
  typescript = {'eslint', 'tsserver'},
  typescriptreact = {'eslint', 'tsserver'},
  graphql = {'eslint '},
  sh = {'shellcheck'}
}
cmd 'hi! link ALEError Error'
cmd 'hi! link ALEWarning CodeWarning'
cmd 'hi! link ALEInfo CodeInfo'
cmd 'hi! link ALEErrorSign ErrorSign'
cmd 'hi! link ALEWarningSign WarningSign'
cmd 'hi! link ALEInfoSign InfoSign'
map('', '<A-e>', '<Plug>(ale_fix)', {noremap=false})
map('', '<A-(>', '<Plug>(ale_previous_wrap)', {noremap=false})
map('', '<A-->', '<Plug>(ale_next_wrap)', {noremap=false})
