--[[ This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at https://mozilla.org/MPL/2.0/. ]]

-- Config for vassal.nvim

require('vassal').commands({
  [[npm i -g typescript typescript-language-server eslint prettier @prisma/language-server cspell]],
  'cd /opt/lua-language-server/ && ./update.sh',
})