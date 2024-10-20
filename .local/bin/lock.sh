#!/bin/bash

# screen locker

bg_img=/tmp/bg_lock.png
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
