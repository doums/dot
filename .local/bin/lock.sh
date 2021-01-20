#!/bin/bash

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! shotgun --version &> /dev/null; then
  >&2 printf "%bThis script needs %bshotgun%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! gm -help &> /dev/null; then
  >&2 printf "%bThis script needs %bgraphicsmagick%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! i3lock --version &> /dev/null; then
  >&2 printf "%bThis script needs %bi3lock-color%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

bg_lock=/tmp/bg_lock.png
shotgun $bg_lock
gm convert $bg_lock -scale 10% -scale 1000% -colorspace Gray -gamma 0.6 \
  -fill  "#33333355" \
  -draw 'rectangle 1237,707 1324,734' \
  -font "/usr/share/fonts/TTF/Inconsolata-Bold.ttf" -pointsize 26 -fill white \
  -draw 'gravity Center text 0,0 locked' \
  $bg_lock
options="
--insidecolor=ffffff1c
--ringcolor=ffffff3e
--linecolor=ffffff00
--keyhlcolor=00000080
--ringvercolor=00000000
--separatorcolor=22222260
--insidevercolor=0000001c
--ringwrongcolor=00000055
--insidewrongcolor=0000001c
--verifcolor=00000000
--wrongcolor=00000000
--layoutcolor=00000000"
mapfile -t options <<< "$options"
i3lock -i "$bg_lock" "${options[@]}" "$@"
