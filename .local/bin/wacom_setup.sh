#!/bin/bash
#
# Wacom Intuos Pro Medium M PTH-660-N

set -E
set -o pipefail

# get the device id
stylus_id=$(xsetwacom list devices | rg STYLUS | awk '{print $8}')
pad_id=$(xsetwacom list devices | rg PAD | awk '{print $8}')

# screen dimensions is 2560x2880 pixels
# tablette active area is 224x148mm ratio → 1.51
# keep the same physical ratio for the screen area 1800x1189 pixels
# and offset the screen drawing area to the center of the screen
map="1800x1189+380+1200"

# if second monitor is 1080p on left
if xrandr --listactivemonitors | rg -q '\+DP-\d 1920.*x1080'; then
  map="1800x1189+2300+1200"
fi
xsetwacom set "$stylus_id" MapToOutput $map
xsetwacom set "$stylus_id" PanScrollThreshold 6000 # default 2600

# set the pad buttons
#
# TOP                                       BOTTOM
# ┌───┬───┬───┬───┐ ┌───────────┐ ┌───┬───┬───┬───┐
# │ 1 │ 2 │ 3 │ 8 │ │ TOUCHRING │ │ 9 │10 │11 │12 │
# └───┴───┴───┴───┘ │  4 5 13   │ └───┴───┴───┴───┘
#                   └───────────┘

xsetwacom set "$pad_id" Button 1 "key +ctrl z -ctrl" # undo
xsetwacom set "$pad_id" Button 2 "key +ctrl y -ctrl" # redo
xsetwacom set "$pad_id" Button 3 "key +alt n"        # next color in palette
xsetwacom set "$pad_id" Button 8 "key +alt b"        # prev color in palette
xsetwacom set "$pad_id" Button 9 "key +alt j"        # last color in palette
xsetwacom set "$pad_id" Button 10 "key +ctrl"
xsetwacom set "$pad_id" Button 11 "key +shift"
xsetwacom set "$pad_id" Button 9 "key +super"

echo "  ✓"
