#!/bin/bash

# screen locker

bg_img=/tmp/bg_lock.png
shotgun $bg_img
gm convert $bg_img -scale 10% -scale 1000% -colorspace Gray -gamma 0.6 \
  -fill "#33333355" \
  -draw 'rectangle 1375,879 1506,920' \
  -font "/usr/share/fonts/TTF/Inconsolata-Bold.ttf" -pointsize 38 -fill white \
  -draw 'gravity Center text 0,0 locked' \
  $bg_img
  # for 1440p
  # -draw 'rectangle 1237,707 1324,734' -pointsize 26 \

options="
--screen=0
--radius=80
--ring-width=10
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
--layout-color=00000000
--image=$bg_img"
mapfile -t options <<<"$options"

if [ "$1" == nofork ]; then
  # ⚠ i3lock fails to fork when called from the suspend service
  # use `-n &` as a fix
  i3lock "${options[@]}" -n &
else
  i3lock "${options[@]}"
fi
