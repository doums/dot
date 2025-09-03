#!/bin/bash
# pierreD

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

if ! dmenu -v &>/dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD dmenu$RS to work"
  exit 1
fi

actions=(
  "lock"
  "logout"
  "sleep"
  "hibernate"
  "poweroff"
  "reboot"
)
picked=$(printf '%s\n' "${actions[@]}" | dmenu -b -i -p 'session' "$@")

case $picked in
"lock") lock.sh ;;
"logout") pkill -SIGTERM "$DESKTOP_SESSION" ;;
"sleep") systemctl sleep ;;
"hibernate") systemctl hibernate ;;
"poweroff") systemctl poweroff ;;
"reboot") systemctl reboot ;;
esac
