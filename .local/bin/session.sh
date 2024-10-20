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

choice=$(printf "lock\nlogout\nsuspend\npoweroff\nreboot" | dmenu -b -i -p 'session' "$@")
case $choice in
"lock")
  BG_IMG_PATH=/tmp/bg_lock.png lock_pre.sh
  lock.sh
  ;;
"logout") pkill -SIGTERM "$DESKTOP_SESSION" ;;
"suspend")
  BG_IMG_PATH=/tmp/bg_lock.png lock_pre.sh
  systemctl suspend
  ;;
"poweroff") systemctl poweroff ;;
"reboot") systemctl reboot ;;
esac
