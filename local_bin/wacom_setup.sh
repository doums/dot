#!/bin/bash
#
# Wacom Intuos Pro Medium M PTH-660-N

set -E
set -o pipefail

# get the device id
stylus_id=$(xsetwacom list devices | rg STYLUS | awk '{print $8}')
eraser_id=$(xsetwacom list devices | rg ERASER | awk '{print $8}')
pad_id=$(xsetwacom list devices | rg PAD | awk '{print $8}')

# screen dimensions is 3840x2560 pixels
# tablette active area is 224x148mm ratio → 1.51
# keep the same physical ratio for the screen area 2000x1322 pixels
# and offset the screen drawing area to the center of the screen
# <width>x<height>+<Xoffset>+<Yoffset> origin is top left corner
# TODO pick a good y offset
map="2000x1322+920+200"

# if DualUp on left
if xrandr --listactivemonitors | rg -q '\+DP-\d 2560.*2880'; then
  map="2000x1322+3480+1200"
fi
xsetwacom set "$stylus_id" MapToOutput $map
xsetwacom set "$eraser_id" MapToOutput $map
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
xsetwacom set "$pad_id" Button 3 "key '"             # next color in palette
xsetwacom set "$pad_id" Button 8 'key "'             # prev color in palette
xsetwacom set "$pad_id" Button 9 "key +super"
xsetwacom set "$pad_id" Button 10 "key +shift"
xsetwacom set "$pad_id" Button 11 "key +ctrl"
# xsetwacom set "$pad_id" Button 12 "key +ctrl"

echo "  ✓"

# NOTE in Gimp create the corresponding shortcuts for color
#   palette
#   → Edit → Keyboard Shortcuts
#   search for: "use next/previous palette color"
