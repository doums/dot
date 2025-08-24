#!/bin/bash
# pierreD

# script to take screenshot with square selection

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

if ! shotgun --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD shotgun$RS to work"
  exit 1
fi

if ! slop --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD slop$RS to work"
  exit 1
fi

if ! xclip -version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD xclip$RS to work"
  exit 1
fi

read -ra options <<< "$(slop -b 6 --color=245,0,87,0.80 -f "-i %i -g %g")"
shotgun "${options[@]}" - | xclip -t 'image/png' -selection clipboard
