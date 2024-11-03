#!/bin/bash

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

save_dir="$HOME/Pictures/screenshots"

if ! shotgun --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD shotgun$RS to work"
  exit 1
fi

if ! [ -d "$save_dir" ]; then
  mkdir "$save_dir"
fi

printf -v date_time "%(%d-%m-%Y_%Hh%Mm%Ss)T" -1
shotgun "$HOME/Pictures/screenshots/screenshot_$date_time.png"
