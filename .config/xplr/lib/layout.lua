---@diagnostic disable
local xplr = xplr
---@diagnostic enable

xplr.config.layouts.custom = {
  main = {
    Horizontal = {
      config = {
        constraints = {
          { Percentage = 70 },
          { Percentage = 30 },
        },
      },
      splits = {
        {
          Vertical = {
            config = {
              constraints = {
                { Length = 3 },
                { Min = 1 },
                { Length = 3 },
              },
            },
            splits = {
              {
                Horizontal = {
                  config = {
                    constraints = {
                      { Length = 9 },
                      { Min = 1 },
                    },
                  },
                  splits = {
                    { Dynamic = 'custom.ctx4.ui' },
                    { Dynamic = 'custom.hidden_filter' },
                  },
                },
              },
              'Table',
              'InputAndLogs',
            },
          },
        },
        'Selection',
      },
    },
  },
  compact = {
    Vertical = {
      config = {
        constraints = {
          { Length = 3 },
          { Min = 1 },
          { Length = 3 },
        },
      },
      splits = {
        'SortAndFilter',
        'Table',
        'InputAndLogs',
      },
    },
  },
}
