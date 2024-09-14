---@diagnostic disable
local xplr = xplr
---@diagnostic enable

local d = xplr.config.modes.builtin.default

d.key_bindings.on_key.n = {
  help = 'new file/dir',
  messages = {
    'PopMode',
    { SwitchModeBuiltin = 'create' },
  },
}
d.key_bindings.on_key['ctrl-w'] = {
  help = 'switch layout',
  messages = {
    { SwitchModeCustom = 'switch_layout' },
  },
}
d.key_bindings.on_key['ctrl-k'] = {
  help = 'scroll up half',
  messages = {
    'ScrollUpHalf',
  },
}
d.key_bindings.on_key['ctrl-j'] = {
  help = 'scroll down half',
  messages = {
    'ScrollDownHalf',
  },
}
-- "goto" mode → useless
d.key_bindings.on_key.g = nil

-- remap -----
-- open in editor
d.key_bindings.on_key.e =
  xplr.config.modes.builtin.action.key_bindings.on_key['e']
-- move here
d.key_bindings.on_key.v =
  xplr.config.modes.builtin.selection_ops.key_bindings.on_key['m']
-- copy here
d.key_bindings.on_key.p =
  xplr.config.modes.builtin.selection_ops.key_bindings.on_key['c']
-- rename (deep clone as 'r' will be remapped afterwards)
d.key_bindings.on_key['alt-r'] =
  xplr.util.clone(xplr.config.modes.builtin.default.key_bindings.on_key['r'])

local switch_layout_mode = {
  name = 'switch layout',
  layout = 'HelpMenu',
  key_bindings = {
    on_key = {
      ['1'] = {
        help = 'main',
        messages = {
          { SwitchLayoutCustom = 'main' },
          'PopMode',
        },
      },
      ['2'] = {
        help = 'compact',
        messages = {
          { SwitchLayoutCustom = 'compact' },
          'PopMode',
        },
      },
    },
  },
}

xplr.config.modes.custom = {
  switch_layout = switch_layout_mode,
}

xplr.config.modes.builtin.move_to.prompt = '→ '
xplr.config.modes.builtin.copy_to.prompt = '→ '
xplr.config.modes.builtin.create_directory.prompt = '+ '
xplr.config.modes.builtin.create_file.prompt = '+ '

xplr.config.modes.builtin.recover = {
  name = 'recover',
  layout = {
    Static = {
      CustomParagraph = {
        ui = {
          title = nil,
          style = { fg = 'LightMagenta', add_modifiers = { 'Bold', 'Italic' } },
        },
        body = '󰏤 nope',
      },
    },
  },
  key_bindings = {
    default = {
      messages = {},
    },
  },
}
