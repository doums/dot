#!/bin/bash
# pierreD

# screen locker

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

if ! shotgun --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD shotgun$RS to work"
  exit 1
fi

if ! gm -help &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD graphicsmagick$RS to work"
  exit 1
fi

if ! i3lock --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD i3lock-color$RS to work"
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
--inside-color=ffffff1c
--ring-color=ffffff3e
--line-color=ffffff00
--keyhl-color=00000080
--ringver-color=212121ff
--separator-color=22222260
--insidever-color=0000001c
--ringwrong-color=c62828c8
--insidewrong-color=0000001c
--verif-color=00000000
--wrong-color=00000000
--layout-color=00000000"
mapfile -t options <<< "$options"
i3lock -i "$bg_lock" "${options[@]}" "$@"
