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
