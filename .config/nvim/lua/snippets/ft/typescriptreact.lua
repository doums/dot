--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Snippets for TypeScript React (*.tsx)

local ls = require('luasnip')
local s = ls.snippet
local fmta = require('luasnip.extras.fmt').fmta
local i = ls.insert_node
local f = ls.function_node
local filename = require('snippets.utils').filename
local copy = require('snippets.utils').copy

local M = {
  s('log', fmta('console.log(<>);', i(1))),
  s('if', fmta('if (<>) {\n\t<>\n}', { i(1), i(2) })),
  s('ei', fmta('else if (<>) {\n\t<>\n}', { i(1), i(2) })),
  s('el', fmta('else {\n\t<>\n}', i(1))),
  s('tl', fmta('`${<>}`', i(1))),
  s('imd', fmta("import <> from '<>';", { i(1), i(2, 'path') })),
  s('im', fmta("import { <> } from '<>';", { i(1), i(2, 'path') })),
  s(
    'fn',
    fmta(
      'function <> (<>: <>) {\n\t<>\n}',
      { i(1, 'name'), i(2, 'args'), i(3, 'ArgsType'), i(4, 'body') }
    )
  ),
  s(
    'fna',
    fmta(
      'const <> = (<>: {<>}) =>> {\n\t<>\n};',
      { i(1, 'name'), i(2, 'args'), i(3, 'ArgsType'), i(4, 'body') }
    )
  ),
  s(
    'tc',
    fmta(
      'try {\n\t<>\n} catch (<>: any) {\n\t<>\n}',
      { i(1, 'body'), i(2, 'e'), i(3) }
    )
  ),
  s(
    'fnc',
    fmta(
      [[
    import * as React from 'react';

    // https://react-typescript-cheatsheet.netlify.app/docs/basic/getting-started/basic_type_example#useful-react-prop-type-examples
    type Props = {};

    export const <> = (<>: Props) =>> {
      console.log(<>);
      return (
        <>
      );
    };
		]],
      {
        filename(),
        i(1, 'args'),
        copy(1),
        i(0),
      },
      { dedent = true }
    )
  ),
}

return M
