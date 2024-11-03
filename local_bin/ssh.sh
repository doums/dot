#!/bin/bash
# pierreD

# wezterm ssh wrapper

choice=$(printf "loup\nlinode\n"\
        | dmenu -b -i -l 2 -p 'ssh' "$@" -fn 'JetBrainsMonoSZ:pixelsize=25:antialias=true')

case $choice in
  "loup")  wezterm ssh loup.local;;
  "linode")  wezterm ssh linode;;
esac
