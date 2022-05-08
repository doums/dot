--[[ External dependencies

for LSP
  * clangd, language server for C/C++ (Arch Linux package clangd)
  * TypeScript Language Server (npm package - typescript-language-server)
  * rust-analyzer (https://rust-analyzer.github.io/manual.html#rustup)
  * shellcheck, shell script static analysis tool (AUR shellcheck-bin)
  * StyLua, Lua code formatter (AUR stylua)
  * lua-language-server, must be installed in /opt/lua-language-server
  * ESLint and Prettier (npm packages - eslint prettier)
  * Prisma LSP (npm package - @prisma/language-server)
  * LTEX LS https://github.com/valentjn/ltex-ls/releases
  * cspell (npm package)

others: git, ripgrep, fzf, node, npm
-- ]]

-- ALIASES -------------------------------------------------------
local fn = vim.fn
local cmd = vim.cmd
local g = vim.g
local opt = vim.opt
local lsp = vim.lsp
local api = vim.api
local map = vim.keymap.set

-- PLUGINS -------------------------------------------------------
-- auto install paq-nvim
local install_path = fn.stdpath('data') .. '/site/pack/paqs/opt/paq-nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  cmd(
    '!git clone --depth=1 https://github.com/savq/paq-nvim.git ' .. install_path
  )
end

cmd('packadd paq-nvim') -- Load package

-- update treesitter parsers
local function update_ts_parsers()
  cmd('TSUpdate')
end
require('paq')({
  { 'savq/paq-nvim', opt = true }, -- Let Paq manage itself
  'b3nj5m1n/kommentary',
  'doums/coBra',
  -- 'doums/ponton.nvim',
  -- 'doums/espresso',
  'doums/sae',
  'doums/lsp_spinner.nvim',
  'doums/floaterm.nvim',
  'doums/oterm.nvim',
  'doums/vassal.nvim',
  { 'nvim-treesitter/nvim-treesitter', run = update_ts_parsers },
  'nvim-treesitter/playground',
  'neovim/nvim-lspconfig',
  'jose-elias-alvarez/null-ls.nvim',
  'ray-x/lsp_signature.nvim',
  'simrat39/rust-tools.nvim',
  'hrsh7th/nvim-cmp',
  'hrsh7th/cmp-buffer',
  'hrsh7th/cmp-nvim-lua',
  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/cmp-path',
  'saadparwaiz1/cmp_luasnip',
  'L3MON4D3/LuaSnip',
  'folke/trouble.nvim',
  'brymer-meneses/grammar-guard.nvim',
  'nvim-lua/plenary.nvim', -- dep of telescope.nvim, gitsigns.nvim, null-ls.nvim
  'nvim-lua/popup.nvim', -- dep of telescope.nvim
  'nvim-telescope/telescope.nvim',
  'lewis6991/gitsigns.nvim',
  'pantharshit00/vim-prisma',
  'kyazdani42/nvim-tree.lua',
  'kyazdani42/nvim-web-devicons', -- dep of nvim-tree.lua
  'ggandor/lightspeed.nvim',
  'AckslD/nvim-neoclip.lua',
  -- paq 'henriquehbr/nvim-startup.lua'
})

-- HELPERS -------------------------------------------------------
-- log util
function _G.dump(...)
  local objects = vim.tbl_map(vim.inspect, { ... })
  print(unpack(objects))
end

local function hl(name, fg, bg, style, sp)
  local hl_map = { fg = fg, bg = bg, sp = sp }
  if type(style) == 'string' then
    hl_map[style] = 1
  elseif type(style) == 'table' then
    for _, v in ipairs(style) do
      hl_map[v] = 1
    end
  end
  api.nvim_set_hl(0, name, hl_map)
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
opt.hidden = true
opt.cursorline = true
opt.cursorlineopt = { 'number', 'screenline' }
opt.switchbuf = 'usetab'
opt.scrolloff = 1
opt.completeopt = { 'menuone', 'noselect' }
opt.pumheight = 10
opt.fillchars = { diff = ' ', fold = ' ', eob = ' ', vert = ' ' }
opt.complete = opt.complete:append({ 'i' })
opt.clipboard = 'unnamedplus'
opt.signcolumn = 'yes:2'
opt.cmdheight = 2
opt.mouse = 'a'
opt.statusline = ' ' -- hide the default statusline on the first frames
opt.laststatus = 3
opt.guifont = 'JetBrains Mono:h16'
opt.guicursor = 'a:block-Caret'
opt.spelllang = 'en_us'
opt.spelloptions = 'camel'
opt.colorcolumn = '66'
opt.textwidth = 66
opt.formatoptions = opt.formatoptions:append('lv')
opt.foldmethod = 'expr'
opt.foldexpr = 'nvim_treesitter#foldexpr()'
opt.foldlevelstart = 99

-- VARIOUS -------------------------------------------------------
-- color scheme
cmd('colorscheme espresso')
-- nvim as man pager
cmd('runtime ftplugin/man.vim')
-- map leader
g.mapleader = ','
-- highlight group for guicursor
hl('Caret', '#2A211C', '#889AFF', 'bold')
hl('WinSeparator', '#332a25', '#332a25')

-- MAPPINGS ------------------------------------------------------
-- c'est en forgeant que l'on devient forgeron
map('', '<Up>', '<Nop>')
map('', '<Down>', '<Nop>')
map('', '<Right>', '<Nop>')
map('', '<Left>', '<Nop>')
-- move fast with Ctrl + hjkl
map('', '<C-l>', '<Plug>SaeRight', { remap = true })
map('', '<C-h>', '<Plug>SaeLeft', { remap = true })
map('', '<C-j>', '<C-d>')
map('', '<C-k>', '<C-u>')
-- move through wrapped line
map('', 'j', 'gj', { silent = true })
map('', 'k', 'gk', { silent = true })
-- goto start and end of line
map('', '<space>l', '$')
map('', '<space>h', '0')
-- command line
map('c', '<A-Right>', '<C-Right>')
map('c', '<A-Left>', '<C-Left>')
map('c', '<A-BS>', '<C-w>')
-- work inner by default
map('o', 'w', 'iw')
-- search and replace
map('v', '<Leader>f', '<Esc>:%s/\\%V')
map('n', '<Leader>f', ':%s/')
-- hide highlight after a search
map('n', '<space>', ':nohlsearch<CR>', { silent = true })
-- show trailing whitespaces
map('n', '<Leader><Space>', '/\\s\\+$<CR>')
-- tabs
map('n', '<Leader>t', ':tabnew<CR>')
map('', '<C-Right>', ':tabn<CR>', { silent = true })
map('', '<C-Left>', ':tabp<CR>', { silent = true })
map('', '<C-Up>', ':+tabmove<CR>', { silent = true })
map('', '<C-Down>', ':-tabmove<CR>', { silent = true })
-- windows
map('n', '<Leader>s', ':new<CR>', { silent = true })
map('n', '<Leader>v', ':vnew<CR>', { silent = true })
map('n', '<Leader><S-s>', ':split<CR>', { silent = true })
map('n', '<Leader><S-v>', ':vsplit<CR>', { silent = true })
map('n', '<A-h>', '<C-w>h', { silent = true })
map('n', '<A-l>', '<C-w>l', { silent = true })
map('n', '<A-j>', '<C-w>j', { silent = true })
map('n', '<A-k>', '<C-w>k', { silent = true })
map('n', '<A-Up>', ':resize +4<CR>', { silent = true })
map('n', '<A-Down>', ':resize -4<CR>', { silent = true })
map('n', '<A-Right>', ':vertical resize +4<CR>', { silent = true })
map('n', '<A-Left>', ':vertical resize -4<CR>', { silent = true })
-- terminal normal mode
map('t', '<Leader>n', '<C-\\><C-N>')
-- toggle spell check
map(
  'n',
  '<F4>',
  [[:lua vim.opt.spell = not vim.opt.spell:get()<CR>]],
  { silent = true }
)

-- AUTOCOMMANDS --------------------------------------------------
local group_id = api.nvim_create_augroup('InitLua', {})
-- whenever CursorHold is fired (nothing typed during
-- `updatetime`) in a normal buffer (&buftype option is empty) run
-- `checktime` to refresh the buffer and retrieve any external
-- changes
api.nvim_create_autocmd('CursorHold', {
  group = group_id,
  pattern = '*',
  callback = function()
    if not opt.buftype:get() then
      cmd('checktime %')
    end
  end,
})
-- disable diagnostics in .env file
api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
  group = group_id,
  pattern = '.env',
  callback = function(arg)
    vim.diagnostic.disable(arg.buf)
  end,
})
-- hide column numbers when viewing man pages
api.nvim_create_autocmd('FileType', {
  group = group_id,
  pattern = 'man',
  command = 'set nonumber',
})
-- highlight the selection when yanking
api.nvim_create_autocmd('TextYankPost', {
  group = group_id,
  pattern = '*',
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- vassal.nvim ---------------------------------------------------
require('vassal').commands({
  [[npm i -g typescript typescript-language-server eslint prettier @prisma/language-server cspell]],
  'cd /opt/lua-language-server/ && ./update.sh',
})

-- ponton.nvim ---------------------------------------------------
hl('StatusLine', nil, '#432717')
hl('StatusLineNC', '#BDAE9D', '#432717')
hl('VertSplit', '#2A190E', nil)
local line_bg = '#432717'
local ponton_cdt = require('ponton.condition')
require('ponton').setup({
  line = {
    'void',
    'mode',
    'buffer_name',
    'buffer_changed',
    'read_only',
    'git_branch',
    'spacer',
    'lsp_spinner',
    'lsp_error',
    'lsp_warning',
    'lsp_information',
    'lsp_hint',
    'line',
    'sep',
    'column',
    'line_percent',
    --[[ 'active_mark_start',
    'active_mark_end', ]]
  },
  segments = {
    mode = {
      map = {
        normal = { '▲', { '#BDAE9D', line_bg, 'bold' } },
        insert = { '◆', { '#049B0A', line_bg, 'bold' } },
        replace = { '◆', { '#C75450', line_bg, 'bold' } },
        visual = { '◆', { '#43A8ED', line_bg, 'bold' } },
        v_line = { '━', { '#43A8ED', line_bg, 'bold' } },
        v_block = { '■', { '#43A8ED', line_bg, 'bold' } },
        select = { '■', { '#3592C4', line_bg, 'bold' } },
        command = { '▼', { '#BDAE9D', line_bg, 'bold' } },
        shell_ex = { '▶', { '#93896C', line_bg, 'bold' } },
        terminal = { '▶', { '#049B0A', line_bg, 'bold' } },
        prompt = { '▼', { '#BDAE9D', line_bg, 'bold' } },
        inactive = { ' ', { line_bg, line_bg } },
      },
      margin = { 1, 1 },
    },
    void = {
      style = { nil, line_bg },
      length = '20%',
    },
    buffer_name = {
      style = { '#BDAE9D', line_bg, 'bold' },
      empty = nil,
      padding = { 1, 1 },
      margin = { 1, 1 },
      decorator = { '❮❰', '❱❯', { '#DF824C', line_bg } },
      conditions = {
        ponton_cdt.buffer_not_empty,
        { ponton_cdt.filetype_not, { 'NvimTree', 'Trouble' } },
      },
    },
    buffer_changed = {
      style = { '#DF824C', line_bg, 'bold' },
      value = '†',
      padding = { nil, 1 },
    },
    read_only = {
      style = { '#C75450', line_bg, 'bold' },
      value = '',
      padding = { nil, 1 },
      conditions = {
        ponton_cdt.is_read_only,
        ponton_cdt.is_normal_buf,
      },
    },
    spacer = { style = { line_bg, line_bg } },
    sep = {
      style = { '#BDAE9D', line_bg },
      text = '⏽',
      conditions = { { ponton_cdt.filetype_not, { 'NvimTree', 'Trouble' } } },
    },
    line_percent = {
      style = { '#BDAE9D', line_bg },
      padding = { nil, 1 },
      conditions = { { ponton_cdt.filetype_not, { 'NvimTree', 'Trouble' } } },
    },
    line = {
      style = { '#BDAE9D', line_bg },
      padding = { 1 },
      conditions = { { ponton_cdt.filetype_not, { 'NvimTree', 'Trouble' } } },
    },
    column = {
      style = { '#BDAE9D', line_bg },
      left_adjusted = true,
      padding = { nil, 1 },
      conditions = { { ponton_cdt.filetype_not, { 'NvimTree', 'Trouble' } } },
    },
    git_branch = {
      style = { '#C5656B', line_bg },
      padding = { 1, 1 },
      prefix = ' ',
      conditions = { { ponton_cdt.filetype_not, 'NvimTree' } },
    },
    lsp_spinner = {
      style = { '#C5656B', line_bg },
      fn = require('lsp_spinner').status,
      padding = { nil, 2 },
      prefix = '󰣪 ',
      conditions = { { ponton_cdt.filetype_not, 'NvimTree' } },
    },
    lsp_error = {
      style = { '#FF0000', line_bg, 'bold' },
      padding = { nil, 1 },
      prefix = '✕',
      conditions = { { ponton_cdt.filetype_not, 'NvimTree' } },
    },
    lsp_warning = {
      style = { '#FFFF00', line_bg, 'bold' },
      padding = { nil, 1 },
      prefix = '▲',
      conditions = { { ponton_cdt.filetype_not, 'NvimTree' } },
    },
    lsp_information = {
      style = { '#FFFFCC', line_bg },
      padding = { nil, 1 },
      prefix = '╸',
      conditions = { { ponton_cdt.filetype_not, 'NvimTree' } },
    },
    lsp_hint = {
      style = { '#F49810', line_bg },
      padding = { nil, 1 },
      prefix = '•',
      conditions = { { ponton_cdt.filetype_not, 'NvimTree' } },
    },
    --[[ active_mark_start = {
      style = { { '#DF824C', line_bg }, { line_bg, line_bg } },
      text = '❱',
    },
    active_mark_end = {
      style = { { '#DF824C', line_bg }, { line_bg, line_bg } },
      text = '❰',
    }, ]]
  },
})

-- kommentary ----------------------------------------------------
g.kommentary_create_default_mappings = false
map('n', '<leader>cc', '<Plug>kommentary_line_default', { remap = true })
map('n', '<leader>c', '<Plug>kommentary_motion_default', { remap = true })
map('v', '<leader>c', '<Plug>kommentary_visual_default', { remap = true })

-- coBra ---------------------------------------------------------
g.coBraPairs = {
  rust = {
    { '<', '>' },
    { '"', '"' },
    { '{', '}' },
    { '(', ')' },
    { '[', ']' },
  },
}
g.coBraDisableCRMap = true

-- floaterm.nvim -------------------------------------------------
require('floaterm').setup({
  layout = 'bottom',
  width = 1,
  height = 0.4,
  bg_color = '#211a16',
})

hl('otermSplit', '#2A190E', '#211a16')
require('oterm').setup({
  bg_color = '#211a16',
  split_hl = 'otermSplit',
})

map('n', '<M-t>', [[<cmd>lua require'oterm'.open()<cr>]])
map('n', '<M-y>', [[<cmd>lua require'oterm'.open({layout='vsplit'})<cr>]])
map(
  'n',
  '<M-n>',
  [[<cmd>lua require'floaterm'.open({name='nnn',layout='center',height=0.7,width=0.6,command='nnn'})<cr>]]
)

-- nvim-tree.lua -------------------------------------------------
local tree_cb = require('nvim-tree.config').nvim_tree_callback
g.nvim_tree_git_hl = 1
map('n', '<Tab>', '<cmd>NvimTreeToggle<CR>')
map('n', '<S-Tab>', '<cmd>NvimTreeFindFile<CR>')
g.nvim_tree_show_icons = { git = 0, folders = 1, files = 1 }
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
}
require('nvim-tree').setup({
  hijack_cursor = true,
  diagnostics = {
    enable = true,
    icons = {
      hint = '',
      info = '',
      warning = '',
      error = '✕',
    },
  },
  actions = {
    open_file = {
      window_picker = {
        exclude = { filetype = { 'Trouble', 'qf' } },
        chars = 'HLJKFQDS',
      },
    },
  },
  view = {
    width = 40,
    mappings = {
      custom_only = true,
      list = {
        { key = { '<CR>', '<2-LeftMouse>' }, cb = tree_cb('edit') },
        { key = { '<2-RightMouse>', '<C-]>' }, cb = tree_cb('cd') },
        { key = '<C-v>', cb = tree_cb('vsplit') },
        { key = '<C-s>', cb = tree_cb('split') },
        { key = '<C-t>', cb = tree_cb('tabnew') },
        { key = '<BS>', cb = tree_cb('close_node') },
        { key = '<S-CR>', cb = tree_cb('close_node') },
        { key = '<Tab>', cb = tree_cb('preview') },
        { key = 'K', cb = tree_cb('first_sibling') },
        { key = 'J', cb = tree_cb('last_sibling') },
        { key = 'I', cb = tree_cb('toggle_ignored') },
        { key = 'H', cb = tree_cb('toggle_dotfiles') },
        { key = 'R', cb = tree_cb('refresh') },
        { key = 'a', cb = tree_cb('create') },
        { key = 'd', cb = tree_cb('remove') },
        { key = 'r', cb = tree_cb('rename') },
        { key = '<C-r>', cb = tree_cb('full_rename') },
        { key = 'x', cb = tree_cb('cut') },
        { key = 'c', cb = tree_cb('copy') },
        { key = 'p', cb = tree_cb('paste') },
        { key = 'y', cb = tree_cb('copy_name') },
        { key = 'Y', cb = tree_cb('copy_path') },
        { key = 'gy', cb = tree_cb('copy_absolute_path') },
        { key = '-', cb = tree_cb('dir_up') },
        { key = 'o', cb = tree_cb('system_open') },
        { key = 'q', cb = tree_cb('close') },
        { key = 'g?', cb = tree_cb('toggle_help') },
      },
    },
  },
})
li('NvimTreeRootFolder', 'Comment')
li('NvimTreeExecFile', 'Todo')
li('NvimTreeSpecialFile', 'Function')
li('NvimTreeFolderIcon', 'Constant')
li('NvimTreeImageFile', 'Normal')
li('NvimTreeGitIgnored', 'Debug')
hl('NvimTreeGitNew', '#42905b', nil, 'italic')
hl('NvimTreeGitStaged', '#39c064', nil, 'italic')
hl('NvimTreeGitRenamed', '#507eae', nil, 'italic')
hl('NvimTreeGitDeleted', '#bd5b5b', nil, 'italic')
li('NvimTreeGitDirty', 'NvimTreeGitDeleted')
hl('NvimTreeWindowPicker', '#BDAE9D', '#2A190E', 'bold')
hl('NvimTreeLspDiagnosticsError', '#FF0000', nil, 'bold')

-- nvim-treesitter -----------------------------------------------
require('nvim-treesitter.configs').setup({
  ensure_installed = {
    'c',
    'cpp',
    'rust',
    'yaml',
    'bash',
    'typescript',
    'javascript',
    'html',
    'css',
    'lua',
    'comment',
    'markdown',
    'jsdoc',
    'tsx',
    'toml',
    'json',
    'graphql',
    'jsonc',
  },
  highlight = { enable = true, custom_captures = { todo = 'Todo' } },
  indent = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'tn',
      node_incremental = '<A-l>',
      scope_incremental = '<A-j>',
      node_decremental = '<A-h>',
    },
  },
})

-- grammar-guard.nvim --------------------------------------------
require('grammar-guard').init()

-- LSP -----------------------------------------------------------
local lspconfig = require('lspconfig')
local lsp_spinner = require('lsp_spinner')
local lsp_signature = require('lsp_signature')
lsp_spinner.setup({
  spinner = { '▪', '■', '□', '▫' },
  interval = 80,
})

fn.sign_define(
  'DiagnosticSignError',
  { text = '✗', texthl = 'DiagnosticSignError' }
)
fn.sign_define(
  'DiagnosticSignWarn',
  { text = '▲', texthl = 'DiagnosticSignWarn' }
)
fn.sign_define(
  'DiagnosticSignInfo',
  { text = '╸', texthl = 'DiagnosticSignInfo' }
)
fn.sign_define(
  'DiagnosticSignHint',
  { text = '•', texthl = 'DiagnosticSignHint' }
)

hl('signatureHint', '#CA7E03', nil, 'italic')
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
  handler_opts = { border = 'none' },
  padding = ' ',
  toggle_key = '<C-q>',
}

local function format_diagnostic(diagnostic)
  return string.format('%s (%s)', diagnostic.message, diagnostic.source)
end

local function prefix_diagnostic(diagnostic)
  local severity_map = {
    [vim.diagnostic.severity.ERROR] = { '✕ ', 'DiagnosticSignError' },
    [vim.diagnostic.severity.WARN] = { '▲ ', 'WarningSign' },
    [vim.diagnostic.severity.INFO] = { '╸ ', 'InformationSign' },
    [vim.diagnostic.severity.HINT] = { '• ', 'HintSign' },
  }
  return unpack(severity_map[diagnostic.severity])
end

-- vim.diagnostic config
vim.diagnostic.config({
  virtual_text = false,
  severity_sort = true,
  float = {
    header = false,
    format = format_diagnostic,
    prefix = prefix_diagnostic,
  },
})

local function on_attach(client, bufnr)
  local bufopt = { buffer = bufnr }
  map('n', '<A-b>', '<cmd>lua vim.lsp.buf.definition()<CR>', bufopt)
  map('n', '<A-S-b>', '<cmd>lua vim.lsp.buf.type_definition()<CR>', bufopt)
  map(
    'n',
    '<A-a>',
    '<cmd>lua vim.diagnostic.goto_prev({float=false})<CR>',
    bufopt
  )
  map(
    'n',
    '<A-z>',
    '<cmd>lua vim.diagnostic.goto_next({float=false})<CR>',
    bufopt
  )
  map('v', '<A-CR>', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', bufopt)
  map('n', '<A-d>', '<cmd>lua vim.lsp.buf.hover()<CR>', bufopt)
  map('n', '<A-g>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', bufopt)
  map(
    'n',
    '<A-i>',
    '<cmd>lua require("rust-tools.inlay_hints").toggle_inlay_hints()<CR>',
    bufopt
  )
  map('n', '<A-r>', '<cmd>lua vim.lsp.buf.rename()<CR>', bufopt)
  map('v', '<A-f>', '<cmd>lua vim.lsp.buf.range_formatting()<CR>', bufopt)
  map('n', '<A-e>', '<cmd>lua vim.lsp.buf.format({async = true})<CR>', bufopt)
  -- open a floating window with the diagnostics from the current cursor position
  api.nvim_create_autocmd('CursorHold', {
    pattern = '*',
    callback = function()
      vim.diagnostic.open_float({ focusable = false, scope = 'cursor' })
    end,
  })
  -- highlight the symbol under the cursor
  if client.server_capabilities.document_highlight then
    api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
      pattern = '<buffer>',
      callback = function()
        lsp.buf.document_highlight()
      end,
    })
    api.nvim_create_autocmd('CursorMoved', {
      pattern = '<buffer>',
      callback = function()
        lsp.buf.clear_references()
      end,
    })
  end
  lsp_spinner.on_attach(client, bufnr)
  lsp_signature.on_attach(signature_help_cfg, bufnr)
end

local capabilities = lsp.protocol.make_client_capabilities()
lsp_spinner.init_capabilities(capabilities)
capabilities.textDocument.completion.completionItem.snippetSupport = true

lspconfig.clangd.setup({ -- C, C++
  on_attach = on_attach,
  capabilities = capabilities,
})
lspconfig.tsserver.setup({ -- TypeScript
  on_attach = function(client, bufnr)
    -- do not use tsserver for formatting (use Prettier through null-ls)
    client.server_capabilities.document_formatting = false
    on_attach(client, bufnr)
  end,
  capabilities = capabilities,
})
require('lspconfig').prismals.setup({
  on_attach = on_attach,
  capabilities = capabilities,
})
-- null-ls.nvim
local null_ls = require('null-ls')
null_ls.setup({
  sources = {
    null_ls.builtins.diagnostics.eslint,
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.diagnostics.cspell,
    null_ls.builtins.formatting.prettier,
    null_ls.builtins.formatting.stylua,
    null_ls.builtins.formatting.prismaFmt,
    null_ls.builtins.completion.spell,
  },
  fallback_severity = vim.diagnostic.severity.HINT,
  on_attach = on_attach,
  capabilities = capabilities,
})
require('rust-tools').setup({ -- Rust
  tools = {
    hover_with_actions = false,
    inlay_hints = {
      autoSetHints = false,
      parameter_hints_prefix = '← ',
      other_hints_prefix = '→ ',
    },
    hover_actions = { border = 'none' },
  },
  server = { -- rust-analyzer server options
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      ['rust-analyzer'] = { checkOnSave = { command = 'clippy' } },
    },
  },
})
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')
lspconfig.sumneko_lua.setup({ -- Lua
  on_attach = on_attach,
  capabilities = capabilities,
  cmd = {
    '/opt/lua-language-server/bin/lua-language-server',
    '-E',
    '/opt/lua-language-server/main.lua',
  },
  settings = {
    Lua = {
      runtime = { version = 'LuaJIT', path = runtime_path },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { 'vim' },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = api.nvim_get_runtime_file('', true),
      },
      telemetry = { enable = false },
    },
  },
})
lspconfig.grammar_guard.setup({
  cmd = { '/opt/ltex-ls/bin/ltex-ls' },
  settings = {
    ltex = {
      enabled = { 'tex', 'markdown', 'plaintext', 'typescript' },
      language = 'en',
      diagnosticSeverity = 'information',
      setenceCacheSize = 2000,
      additionalRules = {
        enablePickyRules = true,
        motherTongue = 'en',
      },
      trace = { server = 'verbose' },
      dictionary = {},
      disabledRules = {},
      hiddenFalsePositives = {},
    },
  },
})

-- Trouble -------------------------------------------------------
require('trouble').setup({
  height = 8,
  indent_lines = false,
  padding = false,
  action_keys = {
    open_split = { '<c-s>' }, -- open buffer in new split
    close_folds = { '<bs>' }, -- close all folds
  },
  signs = {
    error = '✕',
    warning = '▲',
    information = '╸',
    hint = '•',
    other = '╍',
  },
})
map(
  'n',
  '<A-S-q>',
  '<cmd>TroubleToggle workspace_diagnostics<cr>',
  { silent = true }
)
map(
  'n',
  '<A-q>',
  '<cmd>TroubleToggle document_diagnostics<cr>',
  { silent = true }
)
--[[ map('n', '<A-b>', '<cmd>Trouble lsp_definitions<cr>')
map('n', '<A-S-b>', '<cmd>Trouble lsp_type_definitions<cr>') ]]
map('n', '<A-u>', '<cmd>Trouble lsp_references<cr>', { silent = true })
li('TroubleCount', 'Number')
li('TroubleText', 'Fg')
li('TroubleLocation', 'NonText')

-- telescope.nvim ------------------------------------------------
local actions = require('telescope.actions')
require('telescope').setup({
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case',
      '--hidden',
    },
    mappings = {
      i = {
        ['<c-x>'] = false,
        ['<c-s>'] = actions.select_horizontal,
        ['<esc>'] = actions.close, -- <Esc> quit in insert mode
      },
    },
    prompt_prefix = '→ ',
    selection_caret = '❱ ',
    borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
  },
})

_G.dropdown_theme = require('telescope.themes').get_dropdown({
  layout_config = { width = 0.8, height = 15 },
  prompt = ' ',
  previewer = false,
  prompt_title = '',
  border = true,
  borderchars = {
    prompt = { '─', '│', ' ', '│', '┌', '┐', '┘', '└' },
    results = { '─', '│', '─', '│', '├', '┤', '┘', '└' },
    preview = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
  },
})

map('', '<A-s>', '<cmd>Telescope lsp_document_symbols<cr>')
map(
  '',
  '<leader>x',
  '<cmd>lua require("telescope.builtin").find_files{find_command={"fd", "-t", "f"}}<cr>'
)
map('', '<Leader>w', '<cmd>Telescope lsp_workspace_symbols<cr>')
map(
  '',
  '<A-CR>',
  [[<cmd>lua require('telescope.builtin').lsp_code_actions(_G.dropdown_theme)<cr>]]
)
map('', '<C-f>', '<cmd>Telescope live_grep<cr>')
map(
  '',
  '<C-b>',
  [[<cmd>lua require('telescope.builtin').buffers(_G.dropdown_theme)<cr>]]
)
li('TelescopeBorder', 'NonText')

-- nvim-cmp & LuaSnip ------------------------------------------
local cmp = require('cmp')
local luasnip = require('luasnip')

local function has_word_before()
  local line, col = unpack(api.nvim_win_get_cursor(0))
  return col ~= 0
    and api.nvim_buf_get_lines(0, line - 1, line, true)[1]
        :sub(col, col)
        :match('%s')
      == nil
end

local tab_key = cmp.mapping(function(fallback)
  if cmp.visible() then
    cmp.select_next_item()
  elseif luasnip.expand_or_jumpable() then
    luasnip.expand_or_jump()
  elseif has_word_before() then
    cmp.complete()
  else
    fallback()
  end
end, {
  'i',
  'c',
  's',
})

local stab_key = cmp.mapping(function(fallback)
  if cmp.visible() then
    cmp.select_prev_item()
  elseif luasnip.jumpable(-1) then
    luasnip.jump(-1)
  else
    fallback()
  end
end, {
  'i',
  'c',
})

cmp.setup({
  mapping = {
    ['<tab>'] = tab_key,
    ['<S-tab>'] = stab_key,
    ['<M-p>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<M-o>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ['<cr>'] = cmp.mapping.confirm({ select = true }),
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  completion = { completeopt = 'menu,menuone,noinsert' },
  sources = {
    { name = 'luasnip' },
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'path' },
    { name = 'buffer' },
  },
  window = {
    completion = { border = '' },
    documentation = { border = { '', '', '', ' ', '', '', '', ' ' } },
  },
  formatting = {
    format = function(entry, vim_item)
      vim_item.menu = ({
        buffer = '⌈buf⌋',
        nvim_lsp = '⌈lsp⌋',
        luasnip = '⌈snip⌋',
        nvim_lua = '⌈lua⌋',
        path = '⌈path⌋',
      })[entry.source.name]
      return vim_item
    end,
  },
})
-- `/` cmdline setup.
--[[ cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' },
  },
  view = {
    { name = 'wildmenu', separator = '|' },
  },
}) ]]
li('CmpItemAbbr', 'Pmenu')
li('CmpItemAbbrDeprecated', 'Pmenu')
hl('CmpItemAbbrMatch', '#CA7E03', '#432717', 'bold')
hl('CmpItemAbbrMatchFuzzy', '#CA7E03', '#432717', 'bold')
li('CmpItemKind', 'Pmenu')
li('CmpItemMenu', 'Pmenu')

local ps = luasnip.parser.parse_snippet
local js_log = ps({ trig = 'log', name = 'console log' }, 'console.log($0);')
luasnip.snippets = {
  javascript = { js_log },
  typescript = {
    js_log,
    ps('eslint-disable-next-line', [[// eslint-disable-next-line $0]]),
  },
  typescriptreact = { js_log },
  c = { ps('printf', [[printf("$1 -> %s$0\n", $1);]]) },
  rust = {
    ps(
      { trig = 'pprintln', name = 'pretty print debug' },
      [[println!("$1 -> {:#?}", $1);]]
    ),
  },
  lua = {
    ps('print', [[print($0)]]),
    ps(
      { trig = 'dump', name = 'print with vim.inspect' },
      [[print(vim.inspect($0))]]
    ),
    ps(
      { trig = 'format', name = 'string format' },
      [[string.format('%s', $0)]]
    ),
  },
}

-- gitsigns.nvim -------------------------------------------------
require('gitsigns').setup({
  signs = {
    add = { hl = 'GitAddSign', text = '┃' },
    change = { hl = 'GitChangeSign', text = '┃' },
    delete = { hl = 'GitDeleteSign', text = '▶' },
    topdelete = { hl = 'GitDeleteSign', text = '▶' },
    changedelete = { hl = 'GitChangeDeleteSign', text = '┃' },
  },
  numhl = false,
  linehl = false,
  keymaps = {
    noremap = true,
    buffer = true,
    ['n <Leader>n'] = {
      expr = true,
      [[&diff ? '<Leader>n' : '<cmd>lua require"gitsigns".next_hunk()<CR>']],
    },
    ['n <Leader>b'] = {
      expr = true,
      [[&diff ? '<Leader>b' : '<cmd>lua require"gitsigns".prev_hunk()<CR>']],
    },
    ['n <leader>hs'] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
    ['n <leader>hu'] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
    ['n <leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
    ['v <leader>hr'] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    ['n <leader>hR'] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
    ['n <leader>hp'] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
    ['n <leader>hb'] = '<cmd>lua require"gitsigns".blame_line()<CR>',
  },
  preview_config = { border = { '', '', '', ' ', '', '', '', ' ' } },
})

-- lightspeed ----------------------------------------------------
-- stylua: ignore start
require('lightspeed').setup({
  exit_after_idle_msecs = { labeled = 1500, unlabeled = 1500 },
  safe_labels = { 's', 'f', 'n', 'u', 't', 'b', 'g', 'F',
    'L', 'N', 'H', 'G', 'M', 'U', 'T', 'Z' },
  labels = { 's', 'f', 'n', 'g', 'h', 'v', 'b', 'w', 'y', 'd',
    'q', 'z', 'c', 'x', 't', 'u', 'r', 'i', 'a', 'o', 'e' },
})
-- stylua: ignore end
hl('LightspeedCursor', '#212121', '#ebff00', 'bold')
hl('LightspeedLabel', '#f49810', nil, { 'bold', 'underline' })
hl('LightspeedLabelOverlapped', '#f49810', nil, 'underline')
hl('LightspeedShortcut', '#212121', '#f49810', { 'bold', 'underline' })
hl('LightspeedOneCharMatch', '#212121', '#f49810', 'bold')
hl('LightspeedGreyWash', '#80807f')
hl('LightspeedUnlabeledMatch', '#ddddff', nil, 'bold')
hl('LightspeedPendingOpArea', '#212121', '#f49810')
hl('LightspeedLabelDistant', '#aa4e00', nil, { 'bold', 'underline' })
hl('LightspeedLabelDistantOverlapped', '#aa4e00', nil, 'underline')
hl('LightspeedMaskedChar', '#906526')

api.nvim_del_keymap('n', 't')

-- nvim-neoclip.lua ----------------------------------------------
require('neoclip').setup()
map(
  '',
  '<A-c>',
  [[:lua require('telescope').extensions.neoclip.plus()<cr>]],
  { silent = true }
)
