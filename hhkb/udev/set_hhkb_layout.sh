#!/usr/bin/bash

# This script is called by udev rule /etc/udev/rules.d/99-hhkb.rules
# when HHK keyboard is plugged in (USB).

(
if ! xinput --version &> /dev/null; then
  >&2 printf "%s\n" "xinput, command not found"
  exit 1
fi

id=$(xinput list --id-only "Topre Corporation HHKB Professional")
while [ -z "$id" ]; do
  sleep 1
  id=$(xinput list --id-only "Topre Corporation HHKB Professional")
done
setxkbmap -device "$id" -model hhkb -layout hhkb -keycodes hhkb -rules hhkb
echo "✓ HHKB layout loaded"
) &
