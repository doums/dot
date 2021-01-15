#!/bin/bash

perm=(-m 644 -o root -g root)
xkb_dir="/usr/share/X11/xkb"

install "${perm[@]}" conf/00-keyboard.conf /etc/X11/xorg.conf.d/
install "${perm[@]}" compat/hhkb "$xkb_dir/compat"
install "${perm[@]}" keycodes/hhkb "$xkb_dir/keycodes"
install "${perm[@]}" rules/* "$xkb_dir/rules"
install "${perm[@]}" symbols/hhkb "$xkb_dir/symbols"
install "${perm[@]}" types/hhkb "$xkb_dir/types"
