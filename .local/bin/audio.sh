#!/bin/bash

export LC_ALL=C

step=5
app="audio script"
default_sink=0
default_source=1
sink_limit=150
notification_id=111211211
src_notification_id=112112111
icon_sink_high="audio-volume-high"
icon_sink_medium="audio-volume-medium"
icon_sink_low="audio-volume-low"
icon_sink_mute="audio-volume-muted"
icon_source="dialog-information"

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

volume_up() {
  get_volume
  new_volume=$((current_volume + step))
  if [ "$current_volume" = "$sink_limit" ]; then
    new_volume="$sink_limit"
  elif ((new_volume < sink_limit)); then
    pactl set-sink-volume @DEFAULT_SINK@ +"$step"%
  elif ((new_volume >= sink_limit)) && ((current_volume < sink_limit)); then
    pactl set-sink-volume @DEFAULT_SINK@ "$sink_limit"%
    new_volume="$sink_limit"
  fi
  get_sink_icon "$new_volume"
  dunstify -a "$app" -u normal -r "$notification_id" -i "$current_sink_icon" "Volume" "$new_volume%"
}

volume_down() {
  get_volume
  new_volume=$((current_volume - step))
  if ((new_volume < 0)); then
    new_volume=0
  fi
  pactl set-sink-volume @DEFAULT_SINK@ -"$step"%
  get_sink_icon "$new_volume"
  dunstify -a "$app" -u normal -r "$notification_id" -i "$current_sink_icon" "Volume" "$new_volume%"
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
  dunstify -a "$app" -u normal -r "$notification_id" -i "$icon" "Volume" "$body"
}

source_mute() {
  line=$(pactl list sources | rg "Source #$default_source" -A 8 --trim -m 1 | tail -n 1)
  read -ra array <<< "$line"
  if [ "${array[1]}" = yes ]; then
    body="on"
  else
    body="mute"
  fi
  pactl set-source-mute "$default_source" toggle
  dunstify -a "$app" -u normal -r "$src_notification_id" -i "$icon_source" "Micro" "$body"
}

case "$1" in
  "up")
    volume_up
  ;;
  "down")
    volume_down
  ;;
  "sink_mute")
    sink_mute
  ;;
  "source_mute")
    source_mute
  ;;
esac
