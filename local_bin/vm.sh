#!/bin/bash

uri=
[ -z "$uri" ] && echo "✗ connection URI is not set"

vm_n=7
guest=$(printf "arch\neos\ndebian12\npopos22\nubuntu25\nubuntu24\nwin11"\
        | dmenu -b -i -l $vm_n -p 'guest' "$@" -fn 'JetBrainsMono:pixelsize=25:antialias=true')

if [ -z "$guest" ]; then
  exit 0
fi
virt-viewer -d --auto-resize=never -c "$uri" "$guest"
