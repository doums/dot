#!/bin/bash

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! shotgun --version &> /dev/null; then
  >&2 printf "%bThis script needs %bshotgun%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! slop --version &> /dev/null; then
  >&2 printf "%bThis script needs %bslop%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! xclip -version &> /dev/null; then
  >&2 printf "%bThis script needs %bxclip%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

read -ra options <<< "$(slop -b 4 --color=245,0,87,0.80 -f "-i %i -g %g")"
shotgun "${options[@]}" - | xclip -t 'image/png' -selection clipboard
