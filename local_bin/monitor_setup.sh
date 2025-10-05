#!/bin/bash

set -e

primary=eDP-1

mapfile -t monitors < <(xrandr | rg '\bconnected' | awk '{print $1}')
if [ "${#monitors[@]}" -eq 0 ]; then
  echo "[$0] no outputs"
  exit 1
fi

output=$(printf '%s\n' "${monitors[@]}" | dmenu -b -i -p 'monitor' "$@" || true)
if [ -z "$output" ]; then
  exit 0
fi
res=$(xrandr | rg -A1 "\b$output\s+\bconnected" | rg -o '\d+x\d+')
readarray -t modes <<<"$res"
mode="${modes[0]}"
if [ -z "$mode" ]; then
  echo "[$0] failed to find output mode"
  exit 1
fi

positions=(off left right above below)
position=$(printf '%s\n' "${positions[@]}" | dmenu -b -i -p 'position' "$@" || true)
if [ -z "$position" ]; then
  exit 0
fi
if [ "$position" = 'off' ]; then
  xrandr --output "$output" --off
  exit 0
fi

rates=(60 144 240)
rate=$(printf '%s\n' "${rates[@]}" | dmenu -b -i -p 'refresh rate' "$@" || true)
if [ -z "$rate" ]; then
  exit 0
fi

declare -rA flags=(
  [left]="--left-of"
  [right]="--right-of"
  [above]="--above"
  [below]="--below"
)
xrandr --output "$output" --mode "$mode" --rate "$rate" "${flags[$position]}" "$primary"
