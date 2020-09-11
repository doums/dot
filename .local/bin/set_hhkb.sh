#!/usr/bin/bash

# This script is called by the WM when it starts.
# It sets the layout for the HHKB if it is plugged in.

if ! xinput --version &> /dev/null; then
  >&2 printf "%s\n" "xinput, command not found"
  exit 1
fi

if ! xkbcomp -version &> /dev/null; then
  >&2 printf "%s\n" "xkbcomp, command not found"
  exit 1
fi

id=$(xinput list --id-only "Topre Corporation HHKB Professional");
if [ -n "$id" ]; then
  xkbcomp -i "$id" /home/pierre/.local/share/hhkb/hhkb_fr.xkb "$DISPLAY"
fi
