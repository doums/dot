#!/bin/bash
# pierreD

# wezterm ssh wrapper

machines=('loup' 'linode' 'unas')
choice=$(printf "%s\n" "${machines[@]}" |
  dmenu -b -i -l "${#machines}" -p 'ssh' "$@")

case $choice in
"loup") wezterm ssh loup ;;
"linode") wezterm ssh linode ;;
"unas") wezterm ssh unas ;;
esac
