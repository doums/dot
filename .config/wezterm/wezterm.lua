local wezterm = require('wezterm')

local c = wezterm.config_builder()
local act = wezterm.action
local mux = wezterm.mux

c.term = 'wezterm'
c.dpi = 120
c.xcursor_theme = 'Paper'
c.color_scheme = 'Cooper'
c.max_fps = 144
c.adjust_window_size_when_changing_font_size = false

-- font
-- https://github.com/JetBrains/JetBrainsMono/wiki/OpenType-features
-- slashed zero and ligatures off
local font_features = { 'zero', 'ss19', 'calt=0', 'clig=0', 'liga=0' }
c.font = wezterm.font_with_fallback({
  {
    family = 'JetBrains Mono',
    harfbuzz_features = font_features,
  },
  'JetBrainsMono Nerd Font',
})
-- c.allow_square_glyphs_to_overflow_width = 'Always'
c.font_size = 13.0
c.freetype_load_target = 'Light'
c.freetype_render_target = 'HorizontalLcd'
c.char_select_font_size = 14.0
c.custom_block_glyphs = false

-- by default for 'Half' intensity font weight is set to 'ExtraLight'
-- override to 'Regular'
c.font_rules = {
  {
    intensity = 'Half',
    italic = true,
    font = wezterm.font({
      family = 'JetBrains Mono',
      weight = 'Regular',
      style = 'Italic',
      harfbuzz_features = font_features,
    }),
  },
  {
    intensity = 'Half',
    italic = false,
    font = wezterm.font({
      family = 'JetBrains Mono',
      weight = 'Regular',
      harfbuzz_features = font_features,
    }),
  },
}

c.window_padding = {
  left = '1cell',
  right = '1cell',
  top = 6,
  bottom = 6,
}
c.underline_thickness = 3
c.cursor_thickness = 2
c.scrollback_lines = 4096
c.quick_select_alphabet = 'azerty'
c.inactive_pane_hsb = {
  saturation = 1,
  brightness = 1,
}
-- c.window_background_opacity = 0.92
-- c.text_background_opacity = 0.92
c.window_close_confirmation = 'NeverPrompt'
c.hide_mouse_cursor_when_typing = false

-- tab bar
c.use_fancy_tab_bar = false
c.enable_tab_bar = true
c.hide_tab_bar_if_only_one_tab = true
c.tab_bar_at_bottom = true

c.leader = { key = 'w', mods = 'ALT', timeout_milliseconds = 2000 }

-- Show which key table is active in the status area
wezterm.on('update-right-status', function(window, pane)
  local name = window:active_key_table()
  if name then
    name = wezterm.format({
      { Attribute = { Italic = true } },
      { Foreground = { Color = '#ff9e00' } },
      { Text = name .. ' ' },
    })
  end
  window:set_right_status(name or '')
end)

wezterm.on('start-ide', function(window, pane)
  local p_info = pane:get_foreground_process_info()
  local tab, _pane, _ = mux.spawn_window({
    workspace = 'ide',
    cwd = p_info and p_info.cwd or nil,
    args = { 'nvim' },
  })
  _pane:split({
    direction = 'Bottom',
    size = 0.10,
  })
  _pane:activate()
  tab:set_title('IDE')
  mux.set_active_workspace('ide')
end)

c.keys = {
  {
    key = 't',
    mods = 'SHIFT|SUPER',
    action = act.SpawnWindow,
  },
  {
    key = 'Space',
    mods = 'SHIFT|CTRL',
    action = act.ActivateCopyMode,
  },
  {
    key = 'x',
    mods = 'SHIFT|CTRL',
    action = act.QuickSelect,
  },
  {
    key = 'Enter',
    mods = 'ALT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'r',
    mods = 'LEADER',
    action = act.ActivateKeyTable({
      name = 'resize_pane',
      one_shot = false,
    }),
  },
  {
    key = ':',
    mods = 'LEADER',
    action = act.Search('CurrentSelectionOrEmptyString'),
  },
  {
    key = 'v',
    mods = 'LEADER',
    action = act.SplitHorizontal({ domain = 'CurrentPaneDomain' }),
  },
  {
    key = 't',
    mods = 'LEADER',
    action = act.SpawnTab('CurrentPaneDomain'),
  },
  {
    key = 's',
    mods = 'LEADER',
    action = act.SplitVertical({ domain = 'CurrentPaneDomain' }),
  },
  {
    key = 'x',
    mods = 'ALT|SHIFT',
    action = act.CloseCurrentPane({ confirm = false }),
  },
  {
    key = 'x',
    mods = 'LEADER|SHIFT',
    action = act.CloseCurrentTab({ confirm = false }),
  },
  { key = 'PageUp', mods = 'SHIFT', action = act.ScrollByPage(-0.5) },
  { key = 'PageDown', mods = 'SHIFT', action = act.ScrollByPage(0.5) },
  { key = 'h', mods = 'LEADER', action = act.ActivatePaneDirection('Left') },
  { key = 'j', mods = 'LEADER', action = act.ActivatePaneDirection('Down') },
  { key = 'k', mods = 'LEADER', action = act.ActivatePaneDirection('Up') },
  { key = 'l', mods = 'LEADER', action = act.ActivatePaneDirection('Right') },
  { key = 'n', mods = 'LEADER', action = act.EmitEvent('start-ide') },
  {
    key = 'w',
    mods = 'LEADER',
    action = act.ShowLauncherArgs({
      flags = 'WORKSPACES',
      title = 'WORKSPACES',
    }),
  },
  { key = '[', mods = 'LEADER', action = act.ActivateTabRelative(-1) },
  { key = ']', mods = 'LEADER', action = act.ActivateTabRelative(1) },
  { key = 'LeftArrow', mods = 'LEADER', action = act.ActivateTabRelative(-1) },
  { key = 'RightArrow', mods = 'LEADER', action = act.ActivateTabRelative(1) },
}

-- tabs mapping
local tab_keys = { '&', 'é', '"', "'", '(', '-', 'è', '_', 'ç', ')' }
for i, k in ipairs(tab_keys) do
  table.insert(c.keys, {
    key = k,
    mods = 'ALT',
    action = act.ActivateTab(i - 1),
  })
end

-- Search mode keys override (CTRL-SHIFT F)
local search_mode = wezterm.gui.default_key_tables().search_mode
table.insert(
  search_mode,
  { key = 'n', mods = 'CTRL', action = act.CopyMode('PriorMatch') }
)
table.insert(
  search_mode,
  { key = 'n', mods = 'CTRL|SHIFT', action = act.CopyMode('NextMatch') }
)

-- Copy mode keys override (CTRL-SHIFT Space)
local copy_mode = wezterm.gui.default_key_tables().copy_mode
table.insert(
  copy_mode,
  { key = 'i', mods = 'NONE', action = act.CopyMode('Close') }
)
table.insert(
  copy_mode,
  { key = 'l', mods = 'CTRL', action = act.CopyMode('MoveForwardWord') }
)
table.insert(
  copy_mode,
  { key = 'h', mods = 'CTRL', action = act.CopyMode('MoveBackwardWord') }
)
table.insert(copy_mode, {
  key = 'l',
  mods = 'ALT',
  action = act.CopyMode('MoveToEndOfLineContent'),
})
table.insert(copy_mode, {
  key = 'h',
  mods = 'ALT',
  action = act.CopyMode('MoveToStartOfLineContent'),
})
table.insert(copy_mode, {
  key = 'k',
  mods = 'CTRL',
  action = act.CopyMode({ MoveByPage = -0.5 }),
})
table.insert(copy_mode, {
  key = 'j',
  mods = 'CTRL',
  action = act.CopyMode({ MoveByPage = 0.5 }),
})

c.key_tables = {
  resize_pane = {
    { key = 'LeftArrow', action = act.AdjustPaneSize({ 'Left', 1 }) },
    { key = 'h', action = act.AdjustPaneSize({ 'Left', 1 }) },

    { key = 'RightArrow', action = act.AdjustPaneSize({ 'Right', 1 }) },
    { key = 'l', action = act.AdjustPaneSize({ 'Right', 1 }) },

    { key = 'UpArrow', action = act.AdjustPaneSize({ 'Up', 1 }) },
    { key = 'k', action = act.AdjustPaneSize({ 'Up', 1 }) },

    { key = 'DownArrow', action = act.AdjustPaneSize({ 'Down', 1 }) },
    { key = 'j', action = act.AdjustPaneSize({ 'Down', 1 }) },

    -- Cancel the mode by pressing escape
    { key = 'Escape', action = 'PopKeyTable' },
  },
  search_mode = search_mode,
  copy_mode = copy_mode,
}

c.mouse_bindings = {
  -- Change the default click behavior so that it only selects
  -- text and doesn't open hyperlinks
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'NONE',
    action = act.CompleteSelection('ClipboardAndPrimarySelection'),
  },

  -- and make SHIFT-Click open hyperlinks
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'SHIFT',
    action = act.OpenLinkAtMouseCursor,
  },
  {
    event = { Down = { streak = 1, button = 'Left' } },
    mods = 'SHIFT',
    action = act.Nop,
  },
}

-- palette
local p = {
  bg = '#1E1F22',
  -- original fg color from Dark JetBrains IDE theme: '#BCBEC4'
  fg = '#8C8C8C',
  cursor = '#CA7911',
  selection_bg = '#4A17D2',
  selection_fg = '#FFFFFF',
}

c.color_schemes = {
  ['Cooper'] = {
    foreground = p.fg,
    background = p.bg,
    cursor_bg = p.cursor,
    cursor_fg = '#212121',
    cursor_border = p.cursor,
    selection_fg = p.selection_fg,
    selection_bg = p.selection_bg,
    scrollbar_thumb = '#191A1D',
    split = '#161616',

    ansi = {
      '#3B4252', -- black
      '#FF6767', -- red
      '#52B910', -- green
      '#CE993A', -- yellow
      '#6492C1', -- blue
      '#FF2EFF', -- magenta
      '#06B8B8', -- cyan
      '#D6D9E0', -- white
    },
    brights = {
      '#4C566A', -- black
      '#FF4050', -- red
      '#61DA13', -- green
      '#E5BF00', -- yellow
      '#1FB0FF', -- blue
      '#ED7EED', -- magenta
      '#00E5E5', -- cyan
      '#FFFFFF', -- white
    },

    -- When the IME, a dead key or a leader key are being processed and are effectively
    -- holding input pending the result of input composition, change the cursor
    -- to this color to give a visual cue about the compose state.
    compose_cursor = '#FFE49C',

    -- Colors for copy_mode and quick_select
    -- In copy_mode, the color of the active text is:
    -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
    -- 2. selection_* otherwise
    copy_mode_active_highlight_bg = { Color = '#000000' },
    copy_mode_active_highlight_fg = { AnsiColor = 'Black' },
    copy_mode_inactive_highlight_bg = { Color = p.selection_bg },
    copy_mode_inactive_highlight_fg = { AnsiColor = 'White' },

    quick_select_label_bg = { AnsiColor = 'Red' },
    quick_select_label_fg = { Color = 'White' },
    quick_select_match_bg = { Color = p.selection_bg },
    quick_select_match_fg = { Color = p.selection_fg },

    tab_bar = {
      background = p.bg,
      active_tab = {
        bg_color = '#121212',
        fg_color = '#C0C0C0',
        intensity = 'Normal',
      },
      inactive_tab = {
        bg_color = '#1D1D1D',
        fg_color = '#808080',
      },
      inactive_tab_hover = {
        bg_color = '#4A4646',
        fg_color = '#909090',
      },
      new_tab = {
        bg_color = '#1D1D1D',
        fg_color = '#b8a8a8',
      },
      new_tab_hover = {
        bg_color = '#DA7800',
        fg_color = '#B8A8A8',
        intensity = 'Bold',
      },
    },
  },
}

return c
