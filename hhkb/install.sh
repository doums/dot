#!/bin/bash

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

perm=(-m 644 -o root -g root)
xkb_dir="/usr/share/X11/xkb"
udev_rules_dir=/etc/udev/rules.d

if [ "$(id -u)" -ne 0 ]; then
  echo -e " $B_RED✗$RS needs to be run as root" >&2;
  exit 1;
fi

install "${perm[@]}" compat/hhkb "$xkb_dir/compat"
install "${perm[@]}" keycodes/hhkb "$xkb_dir/keycodes"
install "${perm[@]}" rules/* "$xkb_dir/rules"
install "${perm[@]}" symbols/hhkb "$xkb_dir/symbols"
install "${perm[@]}" types/hhkb "$xkb_dir/types"
install udev/set_hhkb_layout.sh /usr/bin
install udev/99-hhkb.rules "$udev_rules_dir"
sed -i "s/_USER_/${SUDO_USER:-$USER}/g" "$udev_rules_dir/99-hhkb.rules"
