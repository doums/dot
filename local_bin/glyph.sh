#!/bin/bash
# pierreD

# select special unicode characters

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

if ! dmenu -v &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD dmenu$RS to work"
  exit 1
fi

choice=$(printf "→ right\n✓ check\n✗ error\n⚠ warn\n⚡ power\n← left\n↑ up\n↓ down\n✶ star\n… 3dots\n⚇ face\n❱ prompt\n❯ prompt thin"\
        | dmenu -b -i -l 11 -p 'glyph' "$@" -fn 'JetBrainsMono:pixelsize=28:antialias=true')
case $choice in
  "→ right")  echo -n → | xclip -selection clipboard;;
  "✓ check")  echo -n ✓ | xclip -selection clipboard;;
  "✗ error")  echo -n ✗ | xclip -selection clipboard;;
  "⚠ warn")   echo -n ⚠ | xclip -selection clipboard;;
  "⚡ power") echo -n ⚡| xclip -selection clipboard;;
  "← left")   echo -n ← | xclip -selection clipboard;;
  "↑ up")     echo -n ↑ | xclip -selection clipboard;;
  "↓ down")   echo -n ↓ | xclip -selection clipboard;;
  "✶ star")   echo -n ✶ | xclip -selection clipboard;;
  "… 3dots")  echo -n … | xclip -selection clipboard;;
  "⚇ face")   echo -n ⚇ | xclip -selection clipboard;;
  "❱ prompt") echo -n ❱ | xclip -selection clipboard;;
  "❯ prompt thin") echo -n ❯ | xclip -selection clipboard;;
esac
