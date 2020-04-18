#!/bin/bash

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! fzf --version &> /dev/null; then
  >&2 printf "%bThis script needs %bfzf%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
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

choice=$(printf "DP above\nDP right\nDP below\nDP left\nDP off" | fzf --no-info --height=6 --min-height=6)
case $choice in
  "DP above") set_DP --above ;;
  "DP right") set_DP --right-of ;;
  "DP below") set_DP --below ;;
  "DP left") set_DP --left-of ;;
  "DP off" )
    xrandr --output DP-2 --off
    feh --no-fehbg --bg-scale "$BG_PRIMARY" "$BG_SECONDARY"
  ;;
esac
