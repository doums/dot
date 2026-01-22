#!/bin/bash

set -e

# Extract text from a screenshot

# required deps:
# - slop
# - shotgun
# - tesseract tesseract-data-eng tesseract-data-fra

sel=$(slop -qlb 6 --color=245,0,87,0.40 -f "-i %i -g %g") || exit 1
read -ra sel <<<"$sel"
text=$(shotgun "${sel[@]}" - |
  tesseract - - -l "eng+fra" --oem 3 --tessdata-dir /usr/share/tessdata)
echo "$text" | xclip -sel clipboard

# send a desktop notification
maxlen=60
ellips=""
[ "${#text}" -gt $maxlen ] && ellips="…"
text=$(echo "$text" | head -n1)
# shellcheck disable=SC1111
notify-send -a cliptext -i xclipboard "Copied into clipboard" "“${text:0:$maxlen}$ellips”"
