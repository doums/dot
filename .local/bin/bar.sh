#!/bin/bash

date_time ()
{
  printf -v date_time "%(%a %-e %B %Y, %-kh%M)T" -1
}

battery ()
{
  bty_path=/sys/class/power_supply/BAT0
  level=$(cat "$bty_path/energy_now")
  capacity=$(cat "$bty_path/energy_full")
  battery=$((100*"$level"/"$capacity"))
}

while :; do
  date_time
  battery
  printf "%d%%   %s\n" "$battery" "$date_time"
	sleep 1
done
