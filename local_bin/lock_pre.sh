#!/bin/bash

# this script is called before suspend and lock actions

shotgun "$BG_IMG_PATH"
gm convert "$BG_IMG_PATH" -scale 10% -scale 1000% -colorspace Gray -gamma 0.6 \
  -fill  "#33333355" \
  -draw 'rectangle 1237,707 1324,734' \
  -font "/usr/share/fonts/TTF/Inconsolata-Bold.ttf" -pointsize 26 -fill white \
  -draw 'gravity Center text 0,0 locked' \
  "$BG_IMG_PATH"
