#!/bin/bash

perm=(-m 644 -o root -g root)
xkb_dir="/usr/share/X11/xkb"

install "${perm[@]}" conf/00-keyboard.conf /etc/X11/xorg.conf.d/
install "${perm[@]}" compat/k7 "$xkb_dir/compat"
install "${perm[@]}" keycodes/k7 "$xkb_dir/keycodes"
install "${perm[@]}" rules/* "$xkb_dir/rules"
install "${perm[@]}" symbols/k7 "$xkb_dir/symbols"
install "${perm[@]}" types/k7 "$xkb_dir/types"
