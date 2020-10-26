#!/bin/bash

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! dmenu -v &> /dev/null; then
  >&2 printf "%bThis script needs %bdemnu%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

choice=$(printf "lock\nlogout\nsuspend\npoweroff\nreboot" | dmenu -b -i -p 'session' "$@")
case $choice in
  "lock") lock.sh;;
  "logout") pkill -SIGTERM "$DESKTOP_SESSION";;
  "suspend") systemctl suspend;;
  "poweroff") systemctl poweroff;;
  "reboot" ) systemctl reboot;;
esac
