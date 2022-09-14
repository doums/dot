--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Snippets for TypeScript

local ls = require('luasnip')
local s = ls.snippet
local fmta = require('luasnip.extras.fmt').fmta
local i = ls.insert_node

local M = {
  s('log', fmta('console.log(<>);', i(0))),
  s('if', fmta('if (<>) {\n\t<>\n}', { i(1), i(0) })),
  s('ei', fmta('else if (<>) {\n\t<>\n}', { i(1), i(0) })),
  s('el', fmta('else {\n\t<>\n}', i(0))),
  s({ trig = 'tl', name = 'Template literal' }, fmta('`${<>}`', i(0))),
  s({ trig = 'tlp', name = 'Template placeholder' }, fmta('${<>}', i(0))),
  s('imd', fmta("import <> from '<>';", { i(1), i(0) })),
  s('im', fmta("import { <> } from '<>';", { i(1), i(0) })),
  s(
    'fn',
    fmta(
      'function <> (<>: <>) {\n\t<>\n}',
      { i(1, 'name'), i(2, 'args'), i(3, 'ArgsType'), i(0) }
    )
  ),
  s(
    'fna',
    fmta(
      'const <> = (<>: {<>}) =>> {\n\t<>\n};',
      { i(1, 'name'), i(2, 'args'), i(3, 'ArgsType'), i(0) }
    )
  ),
  s(
    'tc',
    fmta('try {\n\t<>\n} catch (<>: any) {\n\t<>\n}', { i(1), i(2, 'e'), i(0) })
  ),
  s(
    { trig = 'dst', name = 'Destructuring assignment (object)' },
    fmta('const { <> } = <>;', {
      i(0, 'key'),
      i(1, 'object'),
    })
  ),
  s(
    { trig = 'dsta', name = 'Destructuring assignment (array)' },
    fmta('const [<>] = <>;', {
      i(0, 'index'),
      i(1, 'array'),
    })
  ),
}

return M
