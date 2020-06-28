#!/bin/bash

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! dmenu -v &> /dev/null; then
  >&2 printf "%bThis script needs %bdemnu%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! xrandr --version &> /dev/null; then
  >&2 printf "%bThis script needs %bxrandr%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! feh --version &> /dev/null; then
  >&2 printf "%bThis script needs %bfeh%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

set_DP ()
{
  xrandr --output DP-2 --auto "$1" eDP-1
  feh --no-fehbg --bg-scale "$BG_PRIMARY" "$BG_SECONDARY"
}

choice=$(printf "above\nright\nbelow\nleft\noff" | dmenu -b -i -p 'DP' "$@")
case $choice in
  "above") set_DP --above ;;
  "right") set_DP --right-of ;;
  "below") set_DP --below ;;
  "left") set_DP --left-of ;;
  "off" )
    xrandr --output DP-2 --off
    feh --no-fehbg --bg-scale "$BG_PRIMARY" "$BG_SECONDARY"
  ;;
esac
