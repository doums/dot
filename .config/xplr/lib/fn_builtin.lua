---@diagnostic disable
local xplr = xplr
---@diagnostic enable

local u = require('util')

local nl_icon = '¬'
local row_prefix_style = { fg = 'LightYellow' }

xplr.fn.builtin.fmt_general_selection_item = function(n)
  local nl = xplr.util.paint(nl_icon, { add_modifiers = { 'Italic', 'Dim' } })
  local sh_config = { with_prefix_dots = false, without_suffix_dots = true }
  local shortened = xplr.util.shorten(n.absolute_path, sh_config)
  if n.is_dir then
    shortened = shortened .. '/'
  end
  local meta_style = xplr.util.node_type(n).style
  local ls_style = xplr.util.lscolor(n.absolute_path)
  local style = xplr.util.style_mix({ ls_style, meta_style })
  return xplr.util.paint(shortened:gsub('\n', nl), style)
end

-- Renders the second column in the table
xplr.fn.builtin.fmt_general_table_row_cols_1 = function(m)
  local is_exe = u.is_executable(m)
  local nl = xplr.util.paint(nl_icon, { add_modifiers = { 'Italic', 'Dim' } })
  -- append reset fg color ANSI code to fix an issue with filename colors
  local r = xplr.util.paint(m.prefix, row_prefix_style) .. '\x1b[0m'

  if is_exe then
    r = r
      .. xplr.util.paint(
        '* ',
        { fg = 'LightGreen', add_modifiers = { 'Bold' } }
      )
  elseif m.meta.icon then
    r = r .. xplr.util.paint(m.meta.icon, { fg = m.meta.icon_fg }) .. ' '
  end

  local path = xplr.util.shell_escape(m.relative_path)
  if is_exe then
    if m.is_focused then
      path = xplr.util.paint(
        path,
        { fg = 'LightGreen', add_modifiers = { 'Reversed', 'Italic' } }
      )
    else
      path = xplr.util.paint(
        path,
        { fg = 'LightGreen', add_modifiers = { 'Italic' } }
      )
    end
  else
    path = xplr.util.paint(path, m.style)
  end
  -- if m.is_selected then
  --   path = xplr.util.paint(path, { bg = 'Green' })
  -- else
  --   path = xplr.util.paint(path, m.style)
  -- end
  r = r .. path

  r = r .. m.suffix .. ' '

  if m.is_symlink then
    r = r .. '→ '

    if m.is_broken then
      r = r .. '✗'
    else
      local symlink_path =
        xplr.util.shorten(m.symlink.absolute_path, { base = m.parent })
      if m.symlink.is_dir then
        symlink_path = symlink_path .. '/'
      end
      r = r .. symlink_path:gsub('\n', nl)
    end
  end

  return r
end

-- Renders the third column in the table
xplr.fn.builtin.fmt_general_table_row_cols_2 = function(m)
  local r = xplr.util.paint('r', { fg = 'LightYellow' })
  local w = xplr.util.paint('w', { fg = 'Red' })
  local x =
    xplr.util.paint('x', { fg = 'LightGreen', add_modifiers = { 'Bold' } })
  local s = xplr.util.paint('s', { fg = 'Red' })
  local S = xplr.util.paint('S', { fg = 'Red' })
  local t = xplr.util.paint('t', { fg = 'Red' })
  local T = xplr.util.paint('T', { fg = 'Red' })

  return xplr.util
    .permissions_rwx(m.permissions)
    :gsub('r', r)
    :gsub('w', w)
    :gsub('x', x)
    :gsub('s', s)
    :gsub('S', S)
    :gsub('t', t)
    :gsub('T', T)
end

-- Renders the fourth column in the table
xplr.fn.builtin.fmt_general_table_row_cols_3 = function(m)
  if not m.is_dir then
    return m.human_size
  else
    return ''
  end
end

-- Renders the fifth column in the table
xplr.fn.builtin.fmt_general_table_row_cols_4 = function(m)
  local mtime = m.last_modified / 1000000000
  local current_year = os.date('%Y')
  local mtime_year = os.date('%Y', mtime)
  if current_year == mtime_year then
    return os.date('%e %b %H:%M', mtime)
  else
    return os.date('%e %b %-5Y', mtime)
  end
end
