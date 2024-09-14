---@diagnostic disable
local xplr = xplr
---@diagnostic enable

---@diagnostic disable-next-line: lowercase-global
version = '0.21.7'

local log = xplr.util.debug

-- Lua paths -----
local home = os.getenv('HOME')
package.path = home
  .. '/.config/xplr/lib/?.lua;'
  .. home
  .. '/.config/xplr/plugins/?/init.lua;'
  .. home
  .. '/.config/xplr/plugins/?.lua;'
  .. package.path

-- General Configuration -----
local g = xplr.config.general
local sort_icon_style = {
  fg = 'LightMagenta',
  add_modifiers = { 'Bold' },
}

g.disable_debug_error_mode = false
g.enable_mouse = false
g.show_hidden = false
g.read_only = false
g.enable_recover_mode = true
g.hide_remaps_in_help_menu = false
g.enforce_bounded_index_navigation = false
g.prompt.format = '❱ '
g.prompt.style = { fg = 'Magenta', add_modifiers = { 'Bold' } }
g.logs.info.format = 'INFO'
g.logs.info.style = { fg = 'Blue' }
g.logs.success.format = 'SUCCESS'
g.logs.success.style = { fg = 'Green' }
g.logs.warning.format = 'WARNING'
g.logs.warning.style = { fg = 'Yellow' }
g.logs.error.format = 'ERROR'
g.logs.error.style = { fg = 'Red' }
g.table.header.cols = {}
g.table.header.style = {}
g.table.header.height = 0
g.table.row.cols = {
  { format = 'builtin.fmt_general_table_row_cols_1' },
  { format = 'builtin.fmt_general_table_row_cols_2' },
  { format = 'builtin.fmt_general_table_row_cols_3' },
  { format = 'builtin.fmt_general_table_row_cols_4' },
}
g.table.row.style = {}
g.table.row.height = 0
g.table.style = {}
g.table.tree = { {}, {}, {} }
g.table.col_spacing = 1
g.table.col_widths = {
  { Percentage = 50 },
  { Percentage = 10 },
  { Percentage = 10 },
  { Percentage = 30 },
}
g.selection.item.format = 'builtin.fmt_general_selection_item'
g.selection.item.style = {}
g.search.algorithm = 'Fuzzy'
g.search.unordered = false
g.default_ui.prefix = ' '
g.default_ui.suffix = ''
g.default_ui.style = {
  fg = 'Gray',
}
g.focus_ui.prefix = ' '
g.focus_ui.suffix = ''
g.focus_ui.style = {
  add_modifiers = { 'Reversed' },
}
g.selection_ui.prefix = '❱'
g.selection_ui.suffix = ''
g.selection_ui.style = {}
g.focus_selection_ui.prefix = '❱'
g.focus_selection_ui.suffix = ''
g.focus_selection_ui.style = {
  add_modifiers = { 'Reversed' },
}
g.panel_ui.sort_and_filter.title.format = ''
g.sort_and_filter_ui.separator.format = ' ❯ '
g.sort_and_filter_ui.separator.style = {
  fg = 'DarkGray',
}
g.sort_and_filter_ui.default_identifier.style = {}
g.sort_and_filter_ui.sort_direction_identifiers.forward.format = '↓'
g.sort_and_filter_ui.sort_direction_identifiers.forward.style = sort_icon_style
g.sort_and_filter_ui.sort_direction_identifiers.reverse.format = '↑'
g.sort_and_filter_ui.sort_direction_identifiers.reverse.style = sort_icon_style
g.sort_and_filter_ui.search_identifiers = {
  Fuzzy = { format = '󱐋 ', style = {} },
  Regex = { format = '󰑑 ', style = {} },
}
g.sort_and_filter_ui.search_direction_identifiers.ordered.format = '↓'
g.sort_and_filter_ui.search_direction_identifiers.ordered.style =
  sort_icon_style
g.panel_ui.default.title.style = {
  fg = 'Reset',
  add_modifiers = { 'Bold', 'Reversed' },
}
g.panel_ui.default.borders = nil
g.initial_mode = 'default'
g.initial_layout = 'main'

-- Node Types -----
xplr.config.node_types.directory.style =
  { fg = 'Blue', add_modifiers = { 'Bold' } }
xplr.config.node_types.directory.meta.icon = '󰉋'
xplr.config.node_types.directory.meta.icon_fg = 'Reset'
xplr.config.node_types.file.style = { fg = 'Reset' }
xplr.config.node_types.file.meta.icon = '󰧮'
xplr.config.node_types.file.meta.icon_fg = 'Reset'
xplr.config.node_types.symlink.style = { fg = 'Cyan' }
xplr.config.node_types.symlink.meta.icon = '↔'
xplr.config.node_types.symlink.meta.icon_fg = 'Reset'
xplr.config.node_types.mime_essence = {}
xplr.config.node_types.extension = {}
xplr.config.node_types.special = {}

-- Load local modules -----
require('mode')
require('layout')
require('hacks')
require('fn_builtin')

-- Plugin setup -----
require('zoxide').setup({
  bin = 'zoxide',
  mode = 'default',
  key = 'z',
})

require('ctx4').setup({
  no_title = true,
  keymap = {
    switch_to_1 = '&',
    switch_to_2 = 'é',
    switch_to_3 = '"',
    switch_to_4 = "'",
  },
})

-- Hooks setup -----
return {
  on_load = {
    { LogSuccess = '__loaded' },
    { CallLuaSilently = 'custom.ctx4.hooks.on_load' },
  },
  on_directory_change = {
    { CallLuaSilently = 'custom.ctx4.hooks.on_pwd_change' },
  },
  on_focus_change = {
    { CallLuaSilently = 'custom.ctx4.hooks.on_focus_change' },
  },
  on_mode_switch = {},
  on_layout_switch = {},
}
