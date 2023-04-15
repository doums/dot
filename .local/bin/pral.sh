#!/bin/bash
# pierreD

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# pral is the abbreviation of peripheral

export LC_ALL=C

sink_step=5
brightness_step=5
app="audio script"
default_sink=0
default_source=1
sink_limit=150
sink_id=111211211
source_id=112112111
brightness_id=311221122
icon_sink_high="audio-volume-high"
icon_sink_medium="audio-volume-medium"
icon_sink_low="audio-volume-low"
icon_sink_mute="audio-volume-muted"
icon_source="dialog-information"
sys_path="/sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight"
icon_brightness="preferences-system-brightness-lock"

get_sink_icon() {
  percent=$(("$1" * 100 / sink_limit))
  if ((percent < 30)); then
    current_sink_icon="$icon_sink_low"
  elif ((percent < 70)); then
    current_sink_icon="$icon_sink_medium"
  else
    current_sink_icon="$icon_sink_high"
  fi
} 

get_volume() {
  line=$(pactl list sinks | rg "Sink #$default_sink" -A 9 --trim -m 1 | tail -n 1)
  read -ra array <<< "$line"
  current_volume="${array[4]%%%}"
}

sink_up() {
  get_volume
  new_volume=$((current_volume + sink_step))
  if [ "$current_volume" = "$sink_limit" ]; then
    new_volume="$sink_limit"
  elif ((new_volume < sink_limit)); then
    pactl set-sink-volume @DEFAULT_SINK@ +"$sink_step"%
  elif ((new_volume >= sink_limit)) && ((current_volume < sink_limit)); then
    pactl set-sink-volume @DEFAULT_SINK@ "$sink_limit"%
    new_volume="$sink_limit"
  fi
  get_sink_icon "$new_volume"
  dunstify -a "$app" -u normal -r "$sink_id" -i "$current_sink_icon" "Volume" "$new_volume%"
}

sink_down() {
  get_volume
  new_volume=$((current_volume - sink_step))
  if ((new_volume < 0)); then
    new_volume=0
  fi
  pactl set-sink-volume @DEFAULT_SINK@ -"$sink_step"%
  get_sink_icon "$new_volume"
  dunstify -a "$app" -u normal -r "$sink_id" -i "$current_sink_icon" "Volume" "$new_volume%"
}

sink_mute() {
  line=$(pactl list sinks | rg "Sink #$default_sink" -A 8 --trim -m 1 | tail -n 1)
  read -ra array <<< "$line"
  if [ "${array[1]}" = yes ]; then
    get_volume
    get_sink_icon "$current_volume"
    icon="$current_sink_icon"
    body="on"
  else
    icon="$icon_sink_mute"
    body="off"
  fi
  pactl set-sink-mute @DEFAULT_SINK@ toggle
  dunstify -a "$app" -u normal -r "$sink_id" -i "$icon" "Volume" "$body"
}

source_mute() {
  line=$(pactl list sources | rg "Source #$default_source" -A 8 --trim -m 1 | tail -n 1)
  read -ra array <<< "$line"
  if [ "${array[1]}" = yes ]; then
    body="on"
  else
    body="mute"
  fi
  pactl set-source-mute @DEFAULT_SOURCE@ toggle
  dunstify -a "$app" -u normal -r "$source_id" -i "$icon_source" "Micro" "$body"
}

get_current_brightness() {
  mapfile -tn 1 array < "$sys_path/actual_brightness"
  actual="${array[0]}"
  mapfile -tn 1 array < "$sys_path/max_brightness"
  max="${array[0]}"
  current_brightness=$((100 * actual / max))
}

brightness_up() {
  get_current_brightness
  light -A "$brightness_step"
  new_brightness=$((current_brightness + brightness_step))
  if [ "$new_brightness" -gt 100 ]; then
    new_brightness=100
  fi
  dunstify -a "$app" -u normal -r "$brightness_id" -i "$icon_brightness" "Brightness" "$new_brightness%"
}

brightness_down() {
  get_current_brightness
  light -U "$brightness_step"
  new_brightness=$((current_brightness - brightness_step))
  if [ "$new_brightness" -lt 0 ]; then
    new_brightness=0
  fi
  dunstify -a "$app" -u normal -r "$brightness_id" -i "$icon_brightness" "Brightness" "$new_brightness%"
}

case "$1" in
  "sink_up") sink_up;;
  "sink_down") sink_down;;
  "sink_mute") sink_mute;;
  "source_mute") source_mute;;
  "light_up") brightness_up;;
  "light_down") brightness_down;;
esac
