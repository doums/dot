--[[ External dependencies

for LSP
  * clangd, language server for C/C++ (Arch Linux package clangd)
  * efm, run linters and formatters (Arch Linux package efm-langserver)
  * TypeScript Language Server (npm i -g typescript-language-server)
  * rust-analyzer (https://rust-analyzer.github.io/manual.html#rustup)
  * shellcheck, shell script static analysis tool (AUR shellcheck-bin)
  * LuaFormatter (https://github.com/Koihik/LuaFormatter)
  * lua-language-server, must be installed in /opt/lua-language-server

-- for coq_nvim: python-virtualenv (Arch Linux package) --

others: git, ripgrep, fzf, node, npm
-- ]]

-- ALIASES -------------------------------------------------------
local fn = vim.fn
local cmd = vim.cmd
local g = vim.g
local opt = vim.opt
local lsp = vim.lsp
local api = vim.api

-- PLUGINS -------------------------------------------------------
-- auto install paq-nvim
local install_path = fn.stdpath('data') .. '/site/pack/paqs/opt/paq-nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  cmd('!git clone --depth=1 https://github.com/savq/paq-nvim.git ' ..
        install_path)
end

cmd 'packadd paq-nvim' -- Load package
local paq = require'paq-nvim'.paq

-- update treesitter parsers
local function update_ts_parsers()
  cmd 'TSUpdate'
end

paq {'savq/paq-nvim', opt = true} -- Let Paq manage itself
paq 'b3nj5m1n/kommentary'
paq 'doums/coBra'
paq 'doums/ponton.nvim'
paq 'doums/espresso'
paq 'doums/sae'
paq 'doums/lsp_spinner.nvim'
paq 'doums/lens'
paq 'doums/floaterm.nvim'
paq {'nvim-treesitter/nvim-treesitter', run = update_ts_parsers}
paq 'nvim-treesitter/playground'
paq 'neovim/nvim-lspconfig'
paq 'ray-x/lsp_signature.nvim'
paq 'simrat39/rust-tools.nvim'
paq 'hrsh7th/nvim-compe'
-- paq {'ms-jpq/coq_nvim', branch = 'coq'}
paq 'L3MON4D3/LuaSnip'
paq 'nvim-lua/plenary.nvim' -- dep of telescope.nvim, gitsigns.nvim
paq 'nvim-lua/popup.nvim' -- dep of telescope.nvim
paq 'nvim-telescope/telescope.nvim'
paq 'lewis6991/gitsigns.nvim'
paq 'pantharshit00/vim-prisma'
paq 'kyazdani42/nvim-tree.lua'
paq 'kyazdani42/nvim-web-devicons' -- dep of nvim-tree.lua
paq 'ggandor/lightspeed.nvim'
paq 'AckslD/nvim-neoclip.lua'
-- paq 'henriquehbr/nvim-startup.lua'

-- HELPERS -------------------------------------------------------
-- `t` for `termcodes`.
local function t(str)
  return api.nvim_replace_termcodes(str, true, true, true)
end

-- map with `noremap` option set to `true` by default
local function map(mode, lhs, rhs, opts)
  opts = opts or {noremap = true}
  if opts.noremap == nil then
    opts.noremap = true
  end
  if opts.buffer then
    local bufnr = opts.buffer
    opts.buffer = nil
    api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
  else
    opts.buffer = nil
    api.nvim_set_keymap(mode, lhs, rhs, opts)
  end
end

-- log util
function _G.dump(...)
  local objects = vim.tbl_map(vim.inspect, {...})
  print(unpack(objects))
end

local function hi(name, foreground, background, style, special)
  local fg = 'guifg=' .. (foreground or 'NONE')
  local bg = 'guibg=' .. (background or 'NONE')
  local decoration = 'gui=' .. (style or 'NONE')
  local sp = 'guisp=' .. (special or foreground or 'NONE')
  local hi_command = string.format('hi %s %s %s %s %s', name, fg, bg,
                                   decoration, sp)
  cmd(hi_command)
end

local function li(target, source)
  cmd(string.format('hi! link %s %s', target, source))
end

-- OPTIONS -------------------------------------------------------
opt.termguicolors = true
opt.number = true
opt.relativenumber = true
opt.showmode = false
opt.shortmess = 'IFaWcs'
opt.ignorecase = true
opt.smartcase = true
opt.cindent = true
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.showmatch = true
opt.matchtime = 3
opt.updatetime = 100
opt.splitbelow = true
opt.splitright = true
opt.foldlevelstart = 0
opt.hidden = true
opt.cursorline = true
opt.cursorlineopt = {'number', 'screenline'}
opt.switchbuf = 'usetab'
opt.scrolloff = 1
opt.completeopt = {'menuone', 'noselect'}
opt.pumheight = 10
opt.fillchars = {vert = '┃', diff = ' ', fold = ' ', eob = ' '}
opt.complete = opt.complete:append{'i'}
opt.clipboard = 'unnamedplus'
opt.signcolumn = 'yes:2'
opt.cmdheight = 2
opt.mouse = 'a'
opt.statusline = ' ' -- hide the default statusline on the first frames
opt.guifont = 'JetBrains Mono:h16'
opt.guicursor = 'a:block-Caret'
opt.spelllang = 'en_us'
opt.spelloptions = 'camel'
opt.colorcolumn = '66'

-- VARIOUS -------------------------------------------------------
-- color scheme
cmd 'colorscheme espresso'
-- nvim as man pager
cmd 'runtime ftplugin/man.vim'
-- map leader
g.mapleader = ','
-- highlight group for guicursor
hi('Caret', '#2A211C', '#889AFF', 'bold')

-- MAPPINGS ------------------------------------------------------
-- c'est en forgeant que l'on devient forgeron
map('', '<Up>', '<Nop>')
map('', '<Down>', '<Nop>')
map('', '<Right>', '<Nop>')
map('', '<Left>', '<Nop>')
-- move fast with Ctrl + hjkl
map('', '<C-l>', '<Plug>SaeRight', {noremap = false})
map('', '<C-h>', '<Plug>SaeLeft', {noremap = false})
map('', '<C-j>', '<C-d>')
map('', '<C-k>', '<C-u>')
-- move through wrapped line
map('', 'j', 'gj', {silent = true})
map('', 'k', 'gk', {silent = true})
-- goto start and end of line
map('', '<space>l', '$')
map('', '<space>h', '0')
-- work inner by default
map('o', 'w', 'iw')
-- search and replace
map('v', '<Leader>f', '<Esc>:%s/\\%V')
map('n', '<Leader>f', ':%s/')
-- hide highlight after a search
map('n', '<space>', ':nohlsearch<CR>', {silent = true})
-- show trailing whitespaces
map('n', '<Leader><Space>', '/\\s\\+$<CR>')
-- tabs
map('n', '<Leader>t', ':tabnew<CR>')
map('', '<C-Right>', ':tabn<CR>', {silent = true})
map('', '<C-Left>', ':tabp<CR>', {silent = true})
map('', '<C-Up>', ':+tabmove<CR>', {silent = true})
map('', '<C-Down>', ':-tabmove<CR>', {silent = true})
-- windows
map('n', '<Leader>s', ':new<CR>', {silent = true})
map('n', '<Leader>v', ':vnew<CR>', {silent = true})
map('n', '<Leader><S-s>', ':split<CR>', {silent = true})
map('n', '<Leader><S-v>', ':vsplit<CR>', {silent = true})
map('n', '<A-h>', '<C-w>h', {silent = true})
map('n', '<A-l>', '<C-w>l', {silent = true})
map('n', '<A-j>', '<C-w>j', {silent = true})
map('n', '<A-k>', '<C-w>k', {silent = true})
map('n', '<A-Up>', ':resize +4<CR>', {silent = true})
map('n', '<A-Down>', ':resize -4<CR>', {silent = true})
map('n', '<A-Right>', ':vertical resize +4<CR>', {silent = true})
map('n', '<A-Left>', ':vertical resize -4<CR>', {silent = true})
-- terminal normal mode
map('t', '<Leader>n', '<C-\\><C-N>')
-- toggle spell check
map('n', '<F4>', [[:lua vim.opt.spell = not vim.opt.spell:get()<CR>]],
    {silent = true})

-- AUTOCOMMANDS --------------------------------------------------
-- see https://github.com/neovim/neovim/pull/12378
cmd [[
  augroup init.lua
    autocmd!
    " whenever CursorHold is fired (nothing typed during 'updatetime') in a normal
    " bufer (&buftype option is empty) run checktime to refresh the buffer and
    " retrieve any external changes
    autocmd CursorHold * if empty(&buftype) | checktime % | endif
    autocmd FileType man set nonumber
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup END
]]

-- ponton.nvim ---------------------------------------------------
hi('StatusLineNC', '#BDAE9D', '#432717')
hi('VertSplit', '#2A190E', nil)
local line_bg = '#432717'
require'ponton'.setup({
  line = {
    'active_mark_start', 'mode', 'buffer_name', 'buffer_changed', 'read_only',
    'git_branch', 'spacer', 'lsp_spinner', 'lsp_error', 'lsp_warning',
    'lsp_information', 'lsp_hint', 'line', 'sep', 'column', 'line_percent',
    'active_mark_end',
  },
  segments = {
    mode = {
      map = {
        normal = {'▲', {'#BDAE9D', line_bg, 'bold'}},
        insert = {'◆', {'#049B0A', line_bg, 'bold'}},
        replace = {'◆', {'#C75450', line_bg, 'bold'}},
        visual = {'◆', {'#43A8ED', line_bg, 'bold'}},
        v_line = {'━', {'#43A8ED', line_bg, 'bold'}},
        v_block = {'■', {'#43A8ED', line_bg, 'bold'}},
        select = {'■', {'#3592C4', line_bg, 'bold'}},
        command = {'▼', {'#BDAE9D', line_bg, 'bold'}},
        shell_ex = {'●', {'#93896C', line_bg, 'bold'}},
        terminal = {'●', {'#049B0A', line_bg, 'bold'}},
        prompt = {'▼', {'#BDAE9D', line_bg, 'bold'}},
        inactive = {' ', {line_bg, line_bg}},
      },
      margin = {1, 1},
    },
    buffer_name = {
      style = {'#BDAE9D', '#2A190E', 'bold'},
      empty = nil,
      padding = {1, 1},
      margin = {1, 1},
      decorator = {'', '', {'#2A190E', line_bg}},
      condition = require'ponton.condition'.buffer_not_empty,
    },
    buffer_changed = {
      style = {'#DF824C', line_bg, 'bold'},
      value = '†',
      padding = {nil, 1},
    },
    read_only = {
      style = {'#C75450', line_bg, 'bold'},
      value = '',
      padding = {nil, 1},
      condition = require'ponton.condition'.is_read_only,
    },
    spacer = {style = {line_bg, line_bg}},
    sep = {style = {'#BDAE9D', line_bg}, text = '⏽'},
    line_percent = {style = {'#BDAE9D', line_bg}, padding = {nil, 1}},
    line = {style = {'#BDAE9D', line_bg}, padding = {1}},
    column = {
      style = {'#BDAE9D', line_bg},
      left_adjusted = true,
      padding = {nil, 1},
    },
    git_branch = {
      style = {'#C5656B', line_bg},
      padding = {1, 1},
      prefix = ' ',
    },
    lsp_spinner = {
      style = {'#C5656B', line_bg},
      fn = require'lsp_spinner'.status,
      padding = {nil, 2},
      prefix = '󰣪 ',
    },
    lsp_error = {
      style = {'#FF0000', line_bg, 'bold'},
      padding = {nil, 1},
      prefix = '×',
    },
    lsp_warning = {
      style = {'#FFFF00', line_bg, 'bold'},
      padding = {nil, 1},
      prefix = '•',
    },
    lsp_information = {
      style = {'#FFFFCC', line_bg},
      padding = {nil, 1},
      prefix = '~',
    },
    lsp_hint = {style = {'#F49810', line_bg}, padding = {nil, 1}, prefix = '~'},
    active_mark_start = {
      style = {{'#DF824C', line_bg}, {line_bg, line_bg}},
      text = '▌',
    },
    active_mark_end = {
      style = {{'#DF824C', line_bg}, {line_bg, line_bg}},
      text = '▐',
    },
  },
})

-- kommentary ----------------------------------------------------
g.kommentary_create_default_mappings = false
map('n', '<leader>cc', '<Plug>kommentary_line_default', {noremap = false})
map('n', '<leader>c', '<Plug>kommentary_motion_default', {noremap = false})
map('v', '<leader>c', '<Plug>kommentary_visual_default', {noremap = false})

-- coBra ---------------------------------------------------------
g.coBraPairs = {
  rust = {{'<', '>'}, {'"', '"'}, {'{', '}'}, {'(', ')'}, {'[', ']'}},
}

-- neovide -------------------------------------------------------
g.neovide_refresh_rate = 144
g.neovide_cursor_animation_length = 0.02
g.neovide_cursor_trail_length = 0.6
if g.neovide then
  hi('Error', nil, nil, 'undercurl', '#FF6767')
  hi('SpellBad', nil, nil, 'undercurl', '#659C6B')
  hi('Hint', nil, nil, 'undercurl', '#4E4F4F')
end

-- floaterm.nvim -------------------------------------------------
require'floaterm'.setup {position = 'top', width = 1, height = 0.8}
map('n', '<C-s>', [[<cmd>lua require'floaterm'.find_file()<cr>]])
map('n', '<M-f>', [[<cmd>lua require'floaterm'.rg()<cr>]])

-- nvim-tree.lua -------------------------------------------------
local tree_cb = require'nvim-tree.config'.nvim_tree_callback
g.nvim_tree_width = 40
g.nvim_tree_git_hl = 1
g.nvim_tree_auto_resize = 0
g.nvim_tree_disable_default_keybindings = 1
g.nvim_tree_window_picker_chars = 'HLJKFQDS'
map('n', '<Tab>', '<cmd>NvimTreeToggle<CR>')
map('n', '<S-Tab>', '<cmd>NvimTreeFindFile<CR>')
g.nvim_tree_bindings = {
  {key = {'<CR>', '<2-LeftMouse>'}, cb = tree_cb('edit')},
  {key = {'<2-RightMouse>', '<C-]>'}, cb = tree_cb('cd')},
  {key = '<C-v>', cb = tree_cb('vsplit')},
  {key = '<C-s>', cb = tree_cb('split')},
  {key = '<C-t>', cb = tree_cb('tabnew')},
  {key = 'P', cb = tree_cb('parent_node')},
  {key = '<BS>', cb = tree_cb('close_node')},
  {key = '<S-CR>', cb = tree_cb('close_node')},
  {key = '<Tab>', cb = tree_cb('preview')},
  {key = 'K', cb = tree_cb('first_sibling')},
  {key = 'J', cb = tree_cb('last_sibling')},
  {key = 'I', cb = tree_cb('toggle_ignored')},
  {key = 'H', cb = tree_cb('toggle_dotfiles')},
  {key = 'R', cb = tree_cb('refresh')}, {key = 'a', cb = tree_cb('create')},
  {key = 'd', cb = tree_cb('remove')}, {key = 'r', cb = tree_cb('rename')},
  {key = '<C-r>', cb = tree_cb('full_rename')},
  {key = 'x', cb = tree_cb('cut')}, {key = 'c', cb = tree_cb('copy')},
  {key = 'p', cb = tree_cb('paste')}, {key = 'y', cb = tree_cb('copy_name')},
  {key = 'Y', cb = tree_cb('copy_path')},
  {key = 'gy', cb = tree_cb('copy_absolute_path')},
  {key = '-', cb = tree_cb('dir_up')}, {key = 'o', cb = tree_cb('system_open')},
  {key = 'q', cb = tree_cb('close')}, {key = 'g?', cb = tree_cb('toggle_help')},
}
g.nvim_tree_show_icons = {git = 0, folders = 1, files = 1}
g.nvim_tree_icons = {
  symlink = '󰌹',
  folder = {
    arrow_open = '▼',
    arrow_closed = '▶',
    default = '▶',
    open = '▼',
    empty = '▷',
    empty_open = '▽',
    symlink = '󰌹 ▶',
    symlink_open = '󰌹 ▼',
  },
  git = {
    unstaged = '✗',
    staged = '✓',
    unmerged = '⏼',
    renamed = '→',
    untracked = '•',
    deleted = '-',
    ignored = '⭘',
  },
}
li('NvimTreeRootFolder', 'Comment')
li('NvimTreeExecFile', 'Todo')
li('NvimTreeSpecialFile', 'Function')
li('NvimTreeFolderIcon', 'Constant')
li('NvimTreeImageFile', 'Normal')
li('NvimTreeGitIgnored', 'Debug')
hi('NvimTreeGitNew', '#42905b', nil, 'italic')
hi('NvimTreeGitStaged', '#39c064', nil, 'italic')
hi('NvimTreeGitRenamed', '#507eae', nil, 'italic')
hi('NvimTreeGitDeleted', '#bd5b5b', nil, 'italic')
li('NvimTreeGitDirty', 'NvimTreeGitDeleted')
hi('NvimTreeWindowPicker', '#BDAE9D', '#2A190E', 'bold')

-- nvim-treesitter -----------------------------------------------
require'nvim-treesitter.configs'.setup {
  ensure_installed = {
    'c', 'cpp', 'rust', 'yaml', 'bash', 'typescript', 'javascript', 'html',
    'css', 'lua', 'comment', 'jsdoc', 'tsx', 'toml', 'json', 'graphql', 'jsonc',
  },
  highlight = {enable = true, custom_captures = {todo = 'Todo'}},
  indent = {enable = true},
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'tn',
      node_incremental = '<A-l>',
      scope_incremental = '<A-j>',
      node_decremental = '<A-h>',
    },
  },
}

-- LSP -----------------------------------------------------------
local lspconfig = require 'lspconfig'
local lsp_spinner = require 'lsp_spinner'
local lsp_signature = require 'lsp_signature'
lsp_spinner.setup {
  spinner = {
    '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏',
  },
  interval = 80,
}

fn.sign_define('LspDiagnosticsSignError',
               {text = '▬', texthl = 'LspDiagnosticsSignError'})
fn.sign_define('LspDiagnosticsSignWarning',
               {text = '▬', texthl = 'LspDiagnosticsSignWarning'})
fn.sign_define('LspDiagnosticsSignInformation',
               {text = '▬', texthl = 'LspDiagnosticsSignInformation'})
fn.sign_define('LspDiagnosticsSignHint',
               {text = '▬', texthl = 'LspDiagnosticsSignHint'})

map('n', '<A-a>',
    '<cmd>lua vim.lsp.diagnostic.goto_prev{popup_opts={show_header=false}}<CR>')
map('n', '<A-z>',
    '<cmd>lua vim.lsp.diagnostic.goto_next{popup_opts={show_header=false}}<CR>')
map('v', '<A-CR>', '<cmd>lua vim.lsp.buf.range_code_action()<CR>')
map('n', '<A-t>', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
map('n', '<A-d>', '<cmd>lua vim.lsp.buf.hover()<CR>')
map('n', '<A-r>', '<cmd>lua vim.lsp.buf.rename()<CR>')
map('n', '<A-g>', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
map('n', '<A-i>',
    '<cmd>lua require("rust-tools.inlay_hints").toggle_inlay_hints()<CR>')

hi('signatureHint', '#CA7E03', nil, 'italic')
local signature_help_cfg = {
  bind = true,
  doc_lines = 2,
  floating_window = true,
  hint_enable = true,
  hint_prefix = '→ ',
  hint_scheme = 'signatureHint',
  hi_parameter = 'Search',
  max_height = 4,
  max_width = 80,
  handler_opts = {border = 'none'},
}

local function on_attach(client, bufnr)
  if client.resolved_capabilities.document_range_formatting then
    map('v', '<A-f>', '<cmd>lua vim.lsp.buf.range_formatting()<CR>',
        {buffer = bufnr})
  end
  if client.resolved_capabilities.document_formatting then
    map('n', '<A-e>', '<cmd>lua vim.lsp.buf.formatting()<CR>', {buffer = bufnr})
  end
  -- open a floating window with the diagnostics from the current cursor position
  cmd([[
    augroup lsp_show_line_diagnostics
      autocmd!
      autocmd CursorHold * lua vim.lsp.diagnostic.show_position_diagnostics{show_header=false, focusable = false}
    augroup END
  ]])
  -- highlight the symbol under the cursor
  if client.resolved_capabilities.document_highlight then
    cmd([[
      augroup lsp_document_highlight
        autocmd!
        autocmd CursorHold,CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]])
  end
  lsp_spinner.on_attach(client, bufnr)
  lsp_signature.on_attach(signature_help_cfg)
end

lsp.handlers['textDocument/publishDiagnostics'] =
  lsp.with(lsp.diagnostic.on_publish_diagnostics, {virtual_text = false})

local capabilities = lsp.protocol.make_client_capabilities()
lsp_spinner.init_capabilities(capabilities)
capabilities.textDocument.completion.completionItem.snippetSupport = true

lspconfig.clangd.setup { -- C, C++
  on_attach = on_attach,
  capabilities = capabilities,
}
lspconfig.tsserver.setup { -- TypeScript
  on_attach = function(client, bufnr)
    -- do not use tsserver for formatting (use Prettier through efm)
    client.resolved_capabilities.document_formatting = false
    on_attach(client, bufnr)
  end,
  capabilities = capabilities,
}
require('rust-tools').setup { -- Rust
  tools = {
    hover_with_actions = false,
    inlay_hints = {
      autoSetHints = false,
      parameter_hints_prefix = '← ',
      other_hints_prefix = '→ ',
    },
    hover_actions = {border = 'none'},
  },
  server = { -- rust-analyzer server options
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {['rust-analyzer'] = {checkOnSave = {command = 'clippy'}}},
  },
}
local eslint = {
  lintCommand = 'npx eslint -f visualstudio --stdin --stdin-filename ${INPUT}',
  lintIgnoreExitCode = true,
  lintStdin = true,
  lintFormats = {'%f(%l,%c): %tarning %m', '%f(%l,%c): %rror %m'},
}
local prettier = {
  formatCommand = 'npx prettier --stdin-filepath ${INPUT}',
  formatStdin = true,
}
local languages = {
  javascript = {eslint, prettier},
  javascriptreact = {eslint, prettier},
  typescript = {eslint, prettier},
  typescriptreact = {eslint, prettier},
  yaml = {prettier},
  json = {prettier},
  html = {prettier},
  scss = {prettier},
  css = {prettier},
  markdown = {prettier},
  lua = {{formatCommand = 'lua-format -i', formatStdin = true}},
  sh = {
    {
      lintCommand = 'shellcheck -f gcc -x',
      lintSource = 'shellcheck',
      lintFormats = {
        '%f:%l:%c: %trror: %m', '%f:%l:%c: %tarning: %m', '%f:%l:%c: %tote: %m',
      },
    },
  },
}
lspconfig.efm.setup { -- efm
  init_options = {
    documentFormatting = true,
    -- efm seems to not support range formatting
    -- documentRangeFormatting = true,
    codeAction = true,
  },
  filetypes = vim.tbl_keys(languages),
  on_attach = on_attach,
  settings = {rootMarkers = {'.git/'}, languages = languages},
}
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')
lspconfig.sumneko_lua.setup { -- Lua
  on_attach = on_attach,
  capabilities = capabilities,
  cmd = {
    '/opt/lua-language-server/bin/Linux/lua-language-server', '-E',
    '/opt/lua-language-server/main.lua',
  },
  settings = {
    Lua = {
      runtime = {version = 'LuaJIT', path = runtime_path},
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = api.nvim_get_runtime_file('', true),
      },
      telemetry = {enable = false},
    },
  },
}

-- telescope.nvim ------------------------------------------------
local actions = require('telescope.actions')
require'telescope'.setup {
  defaults = {
    vimgrep_arguments = {
      'rg', '--color=never', '--no-heading', '--with-filename', '--line-number',
      '--column', '--smart-case', '--hidden',
    },
    mappings = {
      i = {
        ['<c-x>'] = false,
        ['<c-s>'] = actions.select_horizontal,
        ['<esc>'] = actions.close, -- <Esc> quit in insert mode
      },
    },
    prompt_prefix = '▶ ',
    selection_caret = '▶ ',
    borderchars = {'─', '│', '─', '│', '┌', '┐', '┘', '└'},
  },
}

map('', '<A-s>', '<cmd>Telescope lsp_document_symbols<cr>')
map('', '<A-x>',
    '<cmd>lua require("telescope.builtin").find_files{find_command={"fd", "-t", "f"}}<cr>')
map('', '<A-w>', '<cmd>Telescope lsp_workspace_symbols<cr>')
map('', '<A-u>', '<cmd>Telescope lsp_references<cr>')
map('', '<A-CR>', '<cmd>lua require"lens".lsp_code_actions()<cr>')
map('', '<A-q>', '<cmd>Telescope lsp_document_diagnostics<cr>')
map('', '<A-S-q>', '<cmd>Telescope lsp_workspace_diagnostics<cr>')
map('', '<A-b>', '<cmd>Telescope lsp_definitions<cr>')
map('', '<C-f>', '<cmd>Telescope live_grep<cr>')
map('', '<C-b>', '<cmd>lua require"lens".buffers()<cr>')
cmd 'hi! link TelescopeBorder NonText'

--[[ -- coq_nvim ------------------------------------------------------
g.coq_settings = {
  auto_start = 'shut-up',
  ['keymap.jump_to_mark'] = '<A-tab>',
  ['display.pum.kind_context'] = {'∙', '∙'},
  ['display.pum.source_context'] = {'⏽', '⏽'},
  ['clients.tree_sitter.enabled'] = false,
  ['clients.tmux.enabled'] = false,
  ['clients.tags.parent_scope'] = ' ↓',
  ['clients.tags.path_sep'] = ' → ',
  ['display.pum.ellipsis'] = '…',
} ]]

-- nvim-compe & LuaSnip ------------------------------------------
local luasnip = require 'luasnip'
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
    spell = true,
    nvim_lsp = true,
    nvim_lua = true,
    luasnip = {priority = 100000},
  },
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
    return t '<C-n>'
  elseif luasnip and luasnip.expand_or_jumpable() then
    return t '<Plug>luasnip-expand-or-jump'
  elseif check_back_space() then
    return t '<Tab>'
  else
    return fn['compe#complete']()
  end
end

function _G.s_tab_complete()
  if fn.pumvisible() == 1 then
    return t '<C-p>'
  elseif luasnip and luasnip.jumpable(-1) then
    return t '<Plug>luasnip-jump-prev'
  else
    return t '<S-Tab>'
  end
end

map('i', '<Tab>', 'v:lua.tab_complete()', {expr = true})
map('s', '<Tab>', 'v:lua.tab_complete()', {expr = true})
map('i', '<S-Tab>', 'v:lua.s_tab_complete()', {expr = true})
map('s', '<S-Tab>', 'v:lua.s_tab_complete()', {expr = true})
map('i', '<C-Space>', 'compe#complete()', {silent = true, expr = true})
map('i', '<CR>', "compe#confirm('<CR>')", {silent = true, expr = true})
map('i', '<C-e>', "compe#close('<C-e>')", {silent = true, expr = true})
map('i', '<A-h>', '<Plug>luasnip-next-choice')
map('s', '<A-h>', '<Plug>luasnip-next-choice')

-- snippets
local ps = luasnip.parser.parse_snippet
local js_log = ps({trig = 'log', name = 'console log'}, 'console.log($0);')
luasnip.snippets = {
  javascript = {js_log},
  typescript = {js_log},
  typescriptreact = {js_log},
  c = {ps('printf', [[printf("$1 -> %s$0\n", $1);]])},
  rust = {
    ps({trig = 'pprintln', name = 'pretty print debug'},
       [[println!("$1 -> {:#?}", $1);]]),
  },
  lua = {
    ps('print', [[print($0)]]),
    ps({trig = 'dump', name = 'print with vim.inspect'},
       [[print(vim.inspect($0))]]),
    ps({trig = 'format', name = 'string format'}, [[string.format('%s', $0)]]),
  },
}

-- gitsigns.nvim -------------------------------------------------
require'gitsigns'.setup {
  signs = {
    add = {hl = 'GitAddSign', text = '┃'},
    change = {hl = 'GitChangeSign', text = '┃'},
    delete = {hl = 'GitDeleteSign', text = '▶'},
    topdelete = {hl = 'GitDeleteSign', text = '▶'},
    changedelete = {hl = 'GitChangeDeleteSign', text = '┃'},
  },
  numhl = false,
  linehl = false,
  keymaps = {
    noremap = true,
    buffer = true,
    ['n <Leader>n'] = {
      expr = true,
      "&diff ? '<Leader>n' : '<cmd>lua require\"gitsigns\".next_hunk()<CR>'",
    },
    ['n <Leader>b'] = {
      expr = true,
      "&diff ? '<Leader>b' : '<cmd>lua require\"gitsigns\".prev_hunk()<CR>'",
    },
    ['n <leader>hs'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['n <leader>hu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['v <leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <leader>hR'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n <leader>hp'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <leader>hb'] = '<cmd>lua require"gitsigns".blame_line()<CR>',
  },
  preview_config = {border = {'', '', '', ' ', '', '', '', ' '}},
}

-- lightspeed ----------------------------------------------------
require'lightspeed'.setup {
  labels = {
    's', 'f', 'g', 'v', 'b', 'n', 'w', 'y', 'd', 'q', 'z', 'c', 'x', 't', 'u',
    'r', 'i', 'a', 'o', 'e',
  },
}
hi('LightspeedCursor', '#212121', '#aeea00', 'bold')

-- nvim-neoclip.lua ----------------------------------------------
require'neoclip'.setup()
map('', '<A-c>', [[:lua require('telescope').extensions.neoclip.default()<cr>]],
    {silent = true})
