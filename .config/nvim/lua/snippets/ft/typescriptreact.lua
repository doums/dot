--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Snippets for TypeScript React (*.tsx)

local ls = require('luasnip')
local s = ls.snippet
local fmt = require('luasnip.extras.fmt').fmt
local fmta = require('luasnip.extras.fmt').fmta
local f = ls.function_node
local i = ls.insert_node
local filename = require('snippets.utils').filename
local copy = require('snippets.utils').copy

local M = {
  s(
    'fnc',
    fmta(
      [[
    import * as React from 'react';

    // https://react-typescript-cheatsheet.netlify.app/docs/basic/getting-started/basic_type_example#useful-react-prop-type-examples

    export const <> = (<>: { <> }) =>> {
      console.log(<>);
      return (
        <>
      );
    };
		]],
      {
        filename(),
        i(1, 'props'),
        i(2, 'type'),
        copy(1),
        i(0),
      },
      { dedent = true }
    )
  ),
  s(
    { trig = 'us', name = 'useState' },
    fmt([[const [{}, {}] = useState<{}>({});]], {
      i(1),
      f(function(args)
        return 'set' .. args[1][1]:gsub('^.', string.upper)
      end, 1),
      i(2),
      i(0),
    })
  ),
  s(
    { trig = 'ue', name = 'useEffect' },
    fmta(
      [[
    useEffect(() =>> {
      <>
    }, [<>]);
		]],
      {
        i(0),
        i(1),
      },
      { dedent = true }
    )
  ),
  s(
    { trig = 'um', name = 'useMemo' },
    fmt([[useMemo(() => {}, [{}]);]], {
      i(0),
      i(1),
    })
  ),
  s(
    { trig = 'uc', name = 'useCallback' },
    fmta(
      [[
    const <> = useCallback(() =>> {
      <>
    }, [<>]);
    ]],
      {
        i(1),
        i(0),
        i(2),
      },
      { dedent = true }
    )
  ),
}

return M
