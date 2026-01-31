#!/bin/bash

if [ -z "$LIBVIRT_URI" ]; then
  echo "LIBVIRT_URI is not set"
  exit 1
fi

vms=(
  'arch'
  'eos'
  'debian12'
  'popos22'
  'ubuntu25'
  'ubuntu24'
  'win11'
)
guest=$(printf "%s\n" "${vms[@]}" |
  dmenu -b -i -l "${#vms[@]}" -p 'guest' "$@")

if [ -z "$guest" ]; then
  exit 0
fi
virt-viewer -d --auto-resize=never -c "$LIBVIRT_URI" "$guest"
