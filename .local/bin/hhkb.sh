#!/usr/bin/bash

# This script is called by udev rule /etc/udev/rules.d/99-hhkb.rules
# when the HHKB keyboard is plugged in (USB).

(
if ! xinput --version &> /dev/null; then
  >&2 printf "%s\n" "xinput, command not found"
  exit 1
fi

if ! xkbcomp -version &> /dev/null; then
  >&2 printf "%s\n" "xkbcomp, command not found"
  exit 1
fi

id=$(xinput list --id-only "Topre Corporation HHKB Professional");
while [ -z "$id" ]; do
  sleep 1
  id=$(xinput list --id-only "Topre Corporation HHKB Professional");
done
xkbcomp -i "$id" /home/pierre/.local/share/hhkb/hhkb_fr.xkb "$DISPLAY"
) &
