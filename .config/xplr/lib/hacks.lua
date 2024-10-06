---@diagnostic disable
local xplr = xplr
---@diagnostic enable

xplr.fn.custom.hidden_filter = function(ctx)
  local filter_hidden = false
  for _, f in ipairs(ctx.app.explorer_config.filters) do
    if string.match(f.filter, 'DoesNotStartWith') and f.input == '.' then
      filter_hidden = true
    end
  end
  return {
    CustomParagraph = {
      ui = { title = nil },
      body = filter_hidden and '' or '',
    },
  }
end

-- toggle exe mode
xplr.config.modes.builtin.default.key_bindings.on_key['*'] = {
  help = 'toggle exe',
  messages = {
    {
      BashExecSilently0 = [===[
        f="$XPLR_FOCUS_PATH"
        if [ -x "$f" ]; then chmod -x "$f"; else chmod +x "$f"; fi
        "$XPLR" -m 'ExplorePwd'
        "$XPLR" -m 'FocusPath: %q' "$f"
      ]===],
    },
  },
}

-- batch rename
-- https://xplr.dev/en/awesome-hacks?highlight=batch#batch-rename
xplr.config.modes.builtin.default.key_bindings.on_key.r = {
  help = 'batch rename',
  messages = {
    {
      BashExec = [===[
        SELECTION=$(cat "${XPLR_PIPE_SELECTION_OUT:?}")
        NODES=${SELECTION:-$(cat "${XPLR_PIPE_DIRECTORY_NODES_OUT:?}")}
        if [ "$NODES" ]; then
          echo -e "$NODES" | renamer
          "$XPLR" -m ExplorePwdAsync
        fi
      ]===],
    },
  },
}

-- On Enter key press, if focused node is a directory enter it,
-- otherwise try to open it
xplr.config.modes.builtin.default.key_bindings.on_key['enter'] = {
  help = 'open or enter',
  messages = {
    {
      BashExecSilently0 = [===[
        P="${XPLR_FOCUS_PATH:?}"
        if [ -d "$P" ]; then
          "$XPLR" -m Enter
        else
          handlr open "$P"
        fi
      ]===],
    },
  },
}

xplr.config.modes.builtin.default.key_bindings.on_key['o'] = {
  help = 'open',
  messages = {
    {
      BashExecSilently0 = [===[
        handlr open "${XPLR_FOCUS_PATH:?}"
      ]===],
    },
  },
}

xplr.config.modes.builtin.default.key_bindings.on_key['i'] = {
  help = 'show image',
  messages = {
    {
      BashExec = [===[
        mime=$(file --mime-type -b "${XPLR_FOCUS_PATH:?}")
        if [[ $mime = image/* ]]; then
          wezterm imgcat "$XPLR_FOCUS_PATH"
          read -r -n1 -p "… "
        else
          "$XPLR" -m 'LogWarning: %q' "not an image: $mime"
        fi
      ]===],
    },
  },
}

-- decompress/list archive using `ouch`
-- https://github.com/ouch-org/ouch
xplr.config.modes.builtin.default.key_bindings.on_key['x'] = {
  help = 'unarchive mode',
  messages = {
    { SwitchModeCustom = 'unarchive' },
    { SetInputBuffer = '' },
  },
}
xplr.config.modes.custom.unarchive = {
  name = 'unarchive',
  prompt = "e'x'tract 'l'ist ",
  key_bindings = {
    on_key = {
      x = {
        help = 'extract',
        messages = {
          {
            BashExec0 = [[
              ouch decompress -A "${XPLR_FOCUS_PATH:?}"
              read -r -n1 -p "… "
            ]],
          },
          'PopMode',
        },
      },
      l = {
        help = 'list',
        messages = {
          {
            BashExec0 = [[
              ouch list "${XPLR_FOCUS_PATH:?}" | ${PAGER:-less}
            ]],
          },
          'PopMode',
        },
      },
      esc = {
        help = 'cancel',
        messages = {
          'PopMode',
        },
      },
    },
    default = {
      messages = {
        'PopMode',
      },
    },
  },
}
