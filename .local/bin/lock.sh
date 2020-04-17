#!/bin/bash

bg_lock=/tmp/bg_lock.png
scrot $bg_lock -o -e 'mv $f /tmp/'
brightness=$(identify -colorspace Gray -gamma 0.6 -crop 300x400+1130+520 -format "%[fx:mean*100]\n" $bg_lock)
brightness=${brightness%.*}
if [ $brightness -gt 60 ]; then
  text_color=black
  args="--insidecolor=0000001c
    --ringcolor=0000003e
    --linecolor=00000000
    --keyhlcolor=ffffff80
    --ringvercolor=ffffff00
    --separatorcolor=22222260
    --insidevercolor=ffffff1c
    --ringwrongcolor=ffffff55
    --insidewrongcolor=ffffff1c
    --verifcolor=00000000
    --wrongcolor=00000000
    --layoutcolor=00000000"
else
  text_color=white
  args="--insidecolor=ffffff1c
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
fi
convert $bg_lock -scale 10% -scale 1000% -colorspace Gray -gamma 0.6 -average -font "Inconsolata-Regular" -pointsize 26 -fill $text_color -gravity Center -annotate +0+0 "locked" $bg_lock
i3lock -n -i $bg_lock $args $@
