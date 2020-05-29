#!/usr/bin/bash

(
if ! xinput --version &> /dev/null; then
  >&2 printf "%s\n" "xinput, command not found"
  exit 1
fi

if ! xkbcomp -version &> /dev/null; then
  >&2 printf "%s\n" "xkbcomp, command not found"
  exit 1
fi

while [ -z "$id" ]; do
  sleep 1
  id=$(xinput list --id-only "Topre Corporation HHKB Professional");
done
xkbcomp -i "$id" /home/pierre/.local/share/hhkb/hhkb_us.xkb "$DISPLAY"
) &
