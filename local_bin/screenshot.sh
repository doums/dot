#!/bin/bash

# required deps:
# - shotgun

save_dir="$HOME/Pictures/screenshots"

if ! [ -d "$save_dir" ]; then
  mkdir "$save_dir"
fi

printf -v date_time "%(%d-%m-%Y_%Hh%Mm%Ss)T" -1
outdir="$HOME/Pictures/screenshots"
file="$outdir/screenshot_$date_time.png"
shotgun "$file" || exit 1

{
  action=$(notify-send -a screenshot -i xclipboard \
    --action="default=open" \
    "screenshot" "Saved in ${file/$HOME/'~'}")
  [ "$action" == "default" ] && xdg-open "$outdir"
} &
