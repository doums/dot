#!/bin/bash
# pierreD

# switch between fr and hhk layouts

set -e

cfg_file="/etc/X11/xorg.conf.d/00-keyboard.conf"

choice=$(printf 'fr\nhhk' | fzf --no-info --header="keyboard layout")
if [ -z "$choice" ]; then
  exit 0
fi

fr_conf='Section "InputClass"
  Identifier "system-keyboard"
  MatchIsKeyboard "on"
  Option "XkbLayout" "fr"
  Option "XkbModel" "pc"
EndSection'

hhk_conf='Section "InputClass"
  Identifier "system-keyboard"
  MatchIsKeyboard "on"
  Option "XkbRules" "hhkb"
  Option "XkbLayout" "hhkb"
  Option "XkbModel" "hhkb"
EndSection'

case $choice in
  "fr")
    echo "$fr_conf" | sudo tee $cfg_file > /dev/null
  ;;
  "hhk")
    echo "$hhk_conf" | sudo tee $cfg_file > /dev/null
  ;;
esac

echo "  ✓ Keyboard layout set to $choice, relog to apply"
