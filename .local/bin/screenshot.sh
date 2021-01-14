#!/bin/bash

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"
save_dir="$HOME/Images/screenshots"

if ! shotgun --version &> /dev/null; then
  >&2 printf "%bThis script needs %bshotgun%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! [ -d "$save_dir" ]; then
  mkdir "$save_dir"
fi

printf -v date_time "%(%d-%m-%Y_%Hh%Mm%Ss)T" -1
shotgun "$HOME/Images/screenshots/screenshot_$date_time.png"
