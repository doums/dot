#!/bin/bash

# pral (abbreviation of peripheral)
# ⚠ require `papirus-icon-theme` to be installed

export LC_ALL=C

sink_step=5
brightness_step=5
app="pral"
sink_limit=100
sink_id=111211211
source_id=112112111
brightness_id=311221122
icon_sink_high="/usr/share/icons/Papirus/48x48/status/notification-audio-volume-high.svg"
icon_sink_medium="/usr/share/icons/Papirus/48x48/status/notification-audio-volume-medium.svg"
icon_sink_low="/usr/share/icons/Papirus/48x48/status/notification-audio-volume-low.svg"
icon_sink_mute="/usr/share/icons/Papirus/48x48/status/notification-audio-volume-muted.svg"
icon_mic_on="/usr/share/icons/Papirus/48x48/status/microphone-sensitivity-high.svg"
icon_mic_mute="/usr/share/icons/Papirus/48x48/status/microphone-sensitivity-muted.svg"
icon_brightness="/usr/share/icons/Papirus/48x48/apps/preferences-system-brightness-lock.svg"
sys_path="/sys/devices/pci0000:00/0000:00:02.0/drm/card1/card1-eDP-1/intel_backlight"

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
  vol=$(pactl get-sink-volume @DEFAULT_SINK@ | rg -o '\d+%' | head -n1)
  current_volume="${vol%%%}" # remove the trailing '%'
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
  dunstify -a "$app" -u normal -r "$sink_id" -I "$current_sink_icon" "Volume" "$new_volume%"
}

sink_down() {
  get_volume
  new_volume=$((current_volume - sink_step))
  if ((new_volume < 0)); then
    new_volume=0
  fi
  pactl set-sink-volume @DEFAULT_SINK@ -"$sink_step"%
  get_sink_icon "$new_volume"
  dunstify -a "$app" -u normal -r "$sink_id" -I "$current_sink_icon" "Volume" "$new_volume%"
}

sink_mute() {
  icon=""
  body=""
  if pactl get-sink-mute @DEFAULT_SINK@ | rg -q "yes"; then
    get_volume
    get_sink_icon "$current_volume"
    icon="$current_sink_icon"
    body="ON"
  else
    icon="$icon_sink_mute"
    body="MUTE"
  fi
  pactl set-sink-mute @DEFAULT_SINK@ toggle
  dunstify -a "$app" -u normal -r "$sink_id" -i "$icon" "Volume" "$body"
}

source_mute() {
  icon=""
  body=""
  if pactl get-source-mute @DEFAULT_SOURCE@ | rg -q "yes"; then
    body="ON"
    icon=$icon_mic_on
  else
    body="MUTE"
    icon=$icon_mic_mute
  fi
  pactl set-source-mute @DEFAULT_SOURCE@ toggle
  dunstify -a "$app" -u normal -r "$source_id" -I $icon "Micro" "$body"
}

get_current_brightness() {
  mapfile -tn 1 array <"$sys_path/actual_brightness"
  actual="${array[0]}"
  mapfile -tn 1 array <"$sys_path/max_brightness"
  max="${array[0]}"
  current_brightness=$((100 * actual / max))
}

# max brightness is 1060, 5% step is 53
brightness_up() {
  get_current_brightness
  brightnessctl set 53+

  new_brightness=$((current_brightness + brightness_step))
  if [ "$new_brightness" -gt 100 ]; then
    new_brightness=100
  fi
  dunstify -a "$app" -u normal -r "$brightness_id" -I "$icon_brightness" "Brightness" "$new_brightness%"
}

brightness_down() {
  get_current_brightness
  brightnessctl set 53-
  new_brightness=$((current_brightness - brightness_step))
  if [ "$new_brightness" -lt 0 ]; then
    new_brightness=0
  fi
  dunstify -a "$app" -u normal -r "$brightness_id" -I "$icon_brightness" "Brightness" "$new_brightness%"
}

case "$1" in
"sink_up") sink_up ;;
"sink_down") sink_down ;;
"sink_mute") sink_mute ;;
"source_mute") source_mute ;;
"light_up") brightness_up ;;
"light_down") brightness_down ;;
esac
