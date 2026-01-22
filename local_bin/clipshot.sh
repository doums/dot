#!/bin/bash

set -e

# Take a screenshot with square selection

# required deps:
# - slop
# - shotgun

sel=$(slop -qlb 6 --color=245,0,87,0.40 -f "-i %i -g %g") || exit 1
read -ra sel <<<"$sel"
shotgun "${sel[@]}" - | xclip -t 'image/png' -selection clipboard

notify-send -a clipshot -i xclipboard "Copied into clipboard"
