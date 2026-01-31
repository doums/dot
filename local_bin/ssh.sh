#!/bin/bash
# pierreD

# wezterm ssh wrapper

choice=$(printf "loup\nlinode\n" |
  dmenu -b -i -l 2 -p 'ssh' "$@")

case $choice in
"loup") wezterm ssh loup ;;
"linode") wezterm ssh linode ;;
esac
