#!/bin/bash

set -e

# Record the screen

# required deps:
# - slop
# - shotgun
# - ffmpeg

printf -v date "%(%d-%m-%Y_%Hh%Mm%Ss)T" -1
outdir="$HOME/Videos/screencasts"
video="$outdir/rec_$date.mkv"

if [ ! -d "$outdir" ]; then
  mkdir -p "$outdir"
fi

sel=$(slop -qlb 6 --color=245,0,87,0.40 -f "%x %y %w %h %g %i") || exit 1
#  shellcheck disable=SC2034
read -r X Y W H G ID <<<"$sel"
# NVIDIA nvenc AV1 codec
# see `ffmpeg -help encoder=av1_nvenc` for options
# https://www.nvidia.com/en-us/geforce/guides/broadcasting-guide/
# ffmpeg -y -f x11grab -s "$W"x"$H" -framerate 60 -i ":0.0+$X,$Y" \
#   -c:v av1_nvenc -preset p3 -tune ull \
#   -rc constqp -qp 20 \
#   -multipass qres \
#   -pix_fmt yuv420p \
#   "$video"
# for better quality (but higher latency?) use
#   -preset p6 -tune hq -qp 15

a_src=alsa_output.pci-0000_00_1f.3-platform-sof_sdw.HiFi__Headphones__sink.monitor
# intel Intel Quick Sync Video AV1 coded
ffmpeg -y -f x11grab -s "$W"x"$H" -framerate 60 -i ":0.0+$X,$Y" \
  -f pulse -ac 2 -i "$a_src" \
  -c:v av1_qsv \
  -preset slow \
  -b:v 6M \
  -maxrate 9M -bufsize 12M \
  -c:a libopus -b:a 128k \
  "$video"

{
  action=$(notify-send -a clipcast -i simplescreenrecorder \
    --action="default=open" \
    "clipcast" "Record saved in ${video/$HOME/'~'}")
  [ "$action" == "default" ] && xdg-open "$outdir"
} &
